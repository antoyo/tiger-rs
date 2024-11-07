/*
 * Copyright (c) 2020 Boucher, Antoni <bouanto@zoho.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

// TODO: use the list encoded in the code to navigate the stack instead of rbp?

use std::arch::asm;
use std::cell::RefCell;
use std::collections::{BTreeMap, HashMap, HashSet};
use std::ffi::CStr;
use std::os::raw::{c_char, c_void};
use std::ptr;

use data_layout::{
    ARRAY_DATA_LAYOUT_SIZE,
    CLASS_DATA_LAYOUT_SIZE,
    RECORD_DATA_LAYOUT_SIZE,
    STRING_DATA_LAYOUT_SIZE,
    STRING_TYPE,
};
use super::{string_offset, WORD_SIZE};

const SHOW_STATS: bool = false;

#[derive(Debug)]
pub enum Layout {
    Array(usize, bool),
    Class(*const c_char),
    Record(*const c_char),
    String(usize),
}

const ARRAY_TYPE: usize = 0;
const RECORD_TYPE: usize = 1;
const CLASS_TYPE: usize = 3;

impl Layout {
    fn write_repr(&self, mut ptr: *mut usize) {
        unsafe {
            match *self {
                Layout::Array(length, is_pointer) => {
                    ptr::write(ptr, ARRAY_TYPE);
                    ptr = ptr.offset(1);
                    ptr::write(ptr, length * WORD_SIZE);
                    ptr = ptr.offset(1);
                    ptr::write(ptr, is_pointer as usize);
                },
                Layout::Class(data_layout) => {
                    ptr::write(ptr, CLASS_TYPE);
                    ptr = ptr.offset(1);
                    ptr::write(ptr, data_layout as usize)
                },
                Layout::Record(data_layout) => {
                    ptr::write(ptr, RECORD_TYPE);
                    ptr = ptr.offset(1);
                    ptr::write(ptr, data_layout as usize)
                },
                Layout::String(length) => {
                    ptr::write(ptr, STRING_TYPE);
                    ptr = ptr.offset(1);
                    ptr::write(ptr, length + 1); // + 1 for the null byte.
                },
            }
        }
    }

    fn size(&self) -> usize {
        match *self {
            Layout::Array(length, _) => (length + ARRAY_DATA_LAYOUT_SIZE) * WORD_SIZE,
            Layout::Class(data_layout) => {
                let string_ptr = string_offset(data_layout);
                let fields = unsafe { CStr::from_ptr(string_ptr) };
                let field_count = fields.to_bytes().len() + CLASS_DATA_LAYOUT_SIZE;
                field_count * WORD_SIZE
            },
            Layout::Record(data_layout) => {
                let string_ptr = string_offset(data_layout);
                let fields = unsafe { CStr::from_ptr(string_ptr) };
                let field_count = fields.to_bytes().len() + RECORD_DATA_LAYOUT_SIZE;
                field_count * WORD_SIZE
            },
            // + 1 for the null byte.
            Layout::String(length) => length + 1 + STRING_DATA_LAYOUT_SIZE * WORD_SIZE,
        }
    }
}

thread_local! {
    pub static GARBAGE_COLLECTOR: RefCell<Collector> = RefCell::new(Collector::new());
}

#[derive(Clone, Debug)]
struct Stack(i64);

impl Stack {
    fn location(&self, base_stack: *const c_void) -> usize {
        (base_stack as i64 + self.0) as usize
    }

    fn to_value(&self, base_stack: *const c_void) -> usize {
        let pointer = (base_stack as i64 + self.0) as *const usize;
        unsafe {
            *pointer
        }
    }
}

pub struct Collector {
    // Free lists from size to index into the heap.
    freelists: BTreeMap<usize, Vec<usize>>,
    freelist_size: HashMap<usize, usize>, // Offset -> size.
    heap: Vec<u8>,
    heap_length: usize,
    marks: HashSet<usize>,
    pointer_map: HashMap<usize, Vec<Stack>>,

    // Stats.
    allocated: usize,
    deallocated: usize,
}

impl Drop for Collector {
    fn drop(&mut self) {
        if SHOW_STATS {
            eprintln!("Allocated {} bytes", self.allocated);
            eprintln!("Deallocated {} bytes", self.deallocated);
        }
    }
}

impl Collector {
    fn new() -> Self {
        let pointer_map = fetch_pointer_map();
        let capacity = std::env::var("TIGER_GC_CAPACITY").map(|str| str.parse().expect("gc capacity")).unwrap_or(4096);
        Self {
            freelists: BTreeMap::new(),
            freelist_size: HashMap::new(),
            heap: vec![0; capacity],
            heap_length: 0,
            marks: HashSet::new(),
            pointer_map,

            allocated: 0,
            deallocated: 0,
        }
    }

    pub fn allocate(&mut self, data_layout: Layout) -> i64 {
        let size = data_layout.size();
        if !self.has_allocation_spot(size) {
            self.collect();
        }
        while !self.has_allocation_spot(size) {
            self.grow_heap();
        }
        let offset = self.grab_allocation_spot(size);
        self.allocated += size;
        unsafe {
            let ptr = self.heap.as_ptr().add(offset) as *mut usize;
            data_layout.write_repr(ptr);
            ptr as i64
        }
    }

    fn collect(&mut self) {
        // Mark.
        let addresses = stack_return_addresses();
        for address in addresses {
            if let Some(roots) = self.pointer_map.get(&(address.return_address as usize)).cloned() {
                for root in roots {
                    self.dfs(root.to_value(address.base_stack));
                }
            }
        }

        // Sweep.
        let start = self.heap.as_ptr() as usize;
        let end = start + self.heap_length - 1;
        let mut pointer = start;
        while pointer <= end {
            let offset = pointer - self.heap.as_ptr() as usize;
            if let Some(size) = self.freelist_size.get(&offset) {
                pointer += size;
                continue;
            }
            let size = size_of(pointer);
            if self.marks.contains(&pointer) {
                self.marks.remove(&pointer);
            }
            else {
                self.deallocated += size;
                self.freelists.entry(size)
                    .or_default()
                    .push(offset);
                self.freelist_size.insert(offset, size);
            }
            pointer += size;
        }
    }

    fn dfs(&mut self, pointer: usize) {
        if self.in_heap(pointer) && !self.marks.contains(&pointer) {
            self.marks.insert(pointer);
            match unsafe { ptr::read_unaligned(pointer as *const usize) } {
                ARRAY_TYPE => {
                    if array_contains_pointers(pointer) {
                        let mut ptr = pointer as *const usize;
                        unsafe {
                            let size = ptr::read_unaligned(ptr.offset(1));
                            ptr = ptr.add(ARRAY_DATA_LAYOUT_SIZE);
                            let end = (ptr as usize + size) as *const usize; // NOTE: the size is the memory size, not the number of elements.
                            while ptr < end {
                                self.dfs(ptr::read_unaligned(ptr));
                                ptr = ptr.offset(1);
                            }
                        }
                    }
                },
                CLASS_TYPE => {
                    for (i, &field_layout) in record_layout(pointer).iter().enumerate() {
                        if field_layout == b'p' {
                            let field = class_field(pointer, i);
                            self.dfs(field);
                        }
                    }
                },
                RECORD_TYPE => {
                    for (i, &field_layout) in record_layout(pointer).iter().enumerate() {
                        if field_layout == b'p' {
                            let field = field(pointer, i);
                            self.dfs(field);
                        }
                    }
                },
                STRING_TYPE => (),
                typ => unreachable!("Invalid type: {}", typ),
            }
        }
    }

    fn dfs_locations(&mut self, pointer: usize, locations: &mut HashMap<usize, usize>) {
        if self.marks.contains(&pointer) {
            return
        }

        self.marks.insert(pointer);

        match unsafe { ptr::read_unaligned(pointer as *const usize) } {
            ARRAY_TYPE => {
                if array_contains_pointers(pointer) {
                    let mut ptr = pointer as *const usize;
                    unsafe {
                        let size = ptr::read_unaligned(ptr.offset(1));
                        ptr = ptr.add(ARRAY_DATA_LAYOUT_SIZE);
                        let end = (ptr as usize + size) as *const usize; // NOTE: the size is the memory size, not the number of elements.
                        while ptr < end {
                            let pointer_value = ptr::read_unaligned(ptr);
                            if self.in_heap(pointer_value) {
                                locations.insert(ptr as usize, pointer_value);
                                self.dfs_locations(pointer_value, locations);
                            }
                            ptr = ptr.offset(1);
                        }
                    }
                }
            },
            CLASS_TYPE => {
                for (i, &field_layout) in record_layout(pointer).iter().enumerate() {
                    if field_layout == b'p' {
                        let field = class_field(pointer, i);
                        if self.in_heap(field) {
                            locations.insert(class_field_address(pointer, i) as usize, field);
                            self.dfs_locations(field, locations);
                        }
                    }
                }
            },
            RECORD_TYPE => {
                for (i, &field_layout) in record_layout(pointer).iter().enumerate() {
                    if field_layout == b'p' {
                        let field = field(pointer, i);
                        if self.in_heap(field) {
                            locations.insert(field_address(pointer, i) as usize, field);
                            self.dfs_locations(field, locations);
                        }
                    }
                }
            },
            STRING_TYPE => (),
            typ => unreachable!("Invalid type: {}", typ),
        }
    }

    fn has_allocation_spot(&self, size: usize) -> bool {
        let has_spot = (self.freelists.contains_key(&size) && !self.freelists[&size].is_empty()) ||
            self.heap_length + size <= self.heap.len();
        if !has_spot {
            for (&key, list) in &self.freelists {
                if key > size && !list.is_empty() {
                    return true;
                }
            }
        }
        has_spot
    }

    fn grab_allocation_spot(&mut self, size: usize) -> usize {
        if self.freelists.contains_key(&size) && !self.freelists[&size].is_empty() {
            let offset = self.freelists.get_mut(&size).expect("allocation spot").pop().expect("allocation spot");
            self.freelist_size.remove(&offset);
            offset
        }
        else if self.heap_length + size <= self.heap.len() {
            let offset = self.heap_length;
            self.heap_length += size;
            offset
        }
        else {
            for (&key, list) in &mut self.freelists {
                if key > size {
                    if let Some(offset) = list.pop() {
                        self.freelist_size.remove(&offset);
                        let new_entry_size = key - size;
                        let new_entry_offset = offset + size;
                        self.freelists.entry(new_entry_size)
                          .or_default()
                          .push(new_entry_offset);

                        self.freelist_size.insert(new_entry_offset, new_entry_size);

                        return offset;
                    }
                }
            }
            unreachable!("no allocation spot");
        }
    }

    fn grow_heap(&mut self) {
        let old_heap = self.heap.as_ptr() as usize;

        let addresses = stack_return_addresses();
        let mut locations = HashMap::new();
        for address in &addresses {
            if let Some(roots) = self.pointer_map.get(&(address.return_address as usize)).cloned() {
                for root in roots {
                    let pointer = root.to_value(address.base_stack);
                    if self.in_heap(pointer) {
                        locations.insert(root.location(address.base_stack), pointer);
                        self.dfs_locations(root.to_value(address.base_stack), &mut locations);
                    }
                }
            }
        }

        self.marks.clear();

        self.heap.resize(self.heap.len() * 2, 0);

        let start = self.heap.as_ptr() as usize;
        if start != old_heap {
            for (&location, &pointer) in &locations {
                let offset = pointer - old_heap;
                unsafe {
                    let location =
                        // NOTE: a location could come from the previous heap, hence this adjustment.
                        if location >= old_heap && location < old_heap + self.heap_length {
                            let offset = location - old_heap;
                            start + offset
                        }
                        else {
                            location
                        };
                    ptr::write(location as *mut usize, start + offset);
                }
            }
        }

        self.marks.clear();
    }

    fn in_heap(&self, value: usize) -> bool {
        let start = self.heap.as_ptr() as usize;
        let end = start + self.heap_length - 1;
        value >= start && value <= end
    }
}

fn array_contains_pointers(ptr: usize) -> bool {
    unsafe {
        let ptr = (ptr as *const usize).offset(2);
        ptr::read_unaligned(ptr) != 0
    }
}

fn fetch_pointer_map() -> HashMap<usize, Vec<Stack>> {
    let mut pointer_map = HashMap::new();
    unsafe {
        let end_marker = &__tiger_pointer_map_end as *const _ as usize;
        let mut pointer = &__tiger_pointer_map as *const usize;
        loop {
            let pointer_val = ptr::read_unaligned(pointer);
            let address =
                if pointer_val == end_marker {
                    break;
                }
                else {
                    pointer_val
                };
            pointer = pointer.offset(1);
            let mut pointers = vec![];
            while ptr::read_unaligned(pointer) != end_marker {
                pointers.push(Stack(ptr::read_unaligned(pointer) as i64));
                pointer = pointer.offset(1);
            }
            pointer = pointer.offset(1);
            pointer_map.insert(address, pointers);
        }
    }
    pointer_map
}

fn class_field(ptr: usize, index: usize) -> usize {
    unsafe {
        ptr::read_unaligned(class_field_address(ptr, index))
    }
}

fn class_field_address(ptr: usize, index: usize) -> *const usize {
    let ptr = ptr as *const usize;
    unsafe {
        ptr.add(index + CLASS_DATA_LAYOUT_SIZE)
    }
}

fn field(ptr: usize, index: usize) -> usize {
    unsafe {
        ptr::read_unaligned(field_address(ptr, index))
    }
}

fn field_address(ptr: usize, index: usize) -> *const usize {
    let ptr = ptr as *const usize;
    unsafe {
        ptr.add(index + RECORD_DATA_LAYOUT_SIZE)
    }
}

fn record_layout(ptr: usize) -> Vec<u8> {
    unsafe {
        let ptr = (ptr as *const usize).offset(1); // Offset 0 is the type, offset 1 is the fields layout.
        let string_ptr = string_offset(ptr::read_unaligned(ptr) as *const c_char);
        let cstring = CStr::from_ptr(string_ptr);
        cstring.to_bytes().to_vec()
    }
}

fn field_count(ptr: usize) -> usize {
    record_layout(ptr).len()
}

fn size_of(ptr: usize) -> usize {
    let ptr = ptr as *const usize;
    unsafe {
        match *ptr {
            ARRAY_TYPE => {
                let ptr = ptr.offset(1);
                ptr::read_unaligned(ptr) + ARRAY_DATA_LAYOUT_SIZE * WORD_SIZE
            },
            CLASS_TYPE => {
                (field_count(ptr as usize) + CLASS_DATA_LAYOUT_SIZE) * WORD_SIZE
            },
            RECORD_TYPE => {
                (field_count(ptr as usize) + RECORD_DATA_LAYOUT_SIZE) * WORD_SIZE
            },
            STRING_TYPE => {
                let ptr = ptr.offset(1);
                ptr::read_unaligned(ptr) + STRING_DATA_LAYOUT_SIZE * WORD_SIZE
            },
            typ => unreachable!("Invalid type: {}", typ),
        }
    }
}

fn rbp() -> usize {
    let result: usize;
    unsafe {
        asm!("mov {result}, rbp", result = out(reg) result)
    }
    result
}

#[derive(Debug)]
struct StackAddresses {
    base_stack: *const c_void,
    return_address: *const c_void,
}

fn stack_return_addresses() -> Vec<StackAddresses> {
    let mut addresses = vec![];
    let mut rbp = rbp() as *const usize;
    unsafe {
        // FIXME: this is probably not correct to have `!= 1` instead of `! … is.null()` (which was
        // the previous code).
        // => It does seem to work, though. Perhaps the value changed in the glibc?
        while rbp as usize != 1 {
            let previous_rbp = *rbp;
            let return_address = *rbp.offset(1) as *const c_void;
            // NOTE: we forced Rust to produce frame pointers to get the whole stacktrace.
            addresses.push(StackAddresses {
                base_stack: previous_rbp as *const c_void,
                return_address,
            });
            rbp = previous_rbp as *const usize;
        }
    }
    addresses
}

extern "C" {
    static __tiger_pointer_map: usize;
    static __tiger_pointer_map_end: usize;
}
