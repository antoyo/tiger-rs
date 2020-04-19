pub struct Logger {
    indent: usize,
}

impl Logger {
    pub fn new() -> Self {
        Self {
            indent: 0,
        }
    }

    pub fn begin_scope(&mut self) {
        self.indent += 4;
    }

    pub fn end_scope(&mut self) {
        self.indent -= 4;
    }

    pub fn print(&self, msg: &str) {
        let indent = " ".repeat(self.indent);
        println!("{}{}", indent, msg);
    }
}
