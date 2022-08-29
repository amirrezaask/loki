
#[derive(Debug)]
pub struct Stack<T> {
    data: Vec<T>
}

impl<T> Stack<T> {
    pub fn new() -> Self {
        Self {
            data: vec![],
        }
    }

    pub fn push(&mut self, elem: T) {
        self.data.push(elem);
    }
    pub fn pop(&mut self) -> &T {
        let last_index = self.data.len()-1;
        let last_elem = &self.data[last_index];
        return last_elem;
    }
    pub fn top(&mut self) -> &T {
        let last_index = self.data.len()-1;

        return &self.data[last_index]; 
    }
}
