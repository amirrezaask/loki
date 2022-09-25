
#[derive(Debug, Clone)]
pub struct Stack<T: Clone + Default> {
    data: Vec<T>
}

impl<T: Clone + Default> Stack<T> {
    pub fn new() -> Self {
        Self {
            data: vec![],
        }
    }

    pub fn len(&self) -> usize {
        return self.data.len();
    }

    pub fn push(&mut self, elem: T) {
        self.data.push(elem);
    }
    pub fn pop(&mut self) -> Option<T> {
        if self.data.len() < 1 {
            return None;
        }
        let last_index = self.data.len()-1;
        let last_elem = self.data[last_index].clone();
        self.data.remove(last_index);
        return Some(last_elem);
    }
    pub fn top(&mut self) -> Option<T> {
        if self.data.len() < 1 {
            return None;
        }
        let last_index = self.data.len()-1;

        return Some(self.data[last_index].clone()); 
    }
}
