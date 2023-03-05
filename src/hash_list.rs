use std::path::Iter;

pub struct HashList<Key, Value>
where
    Key: PartialEq,
{
    pub data: Vec<(Key, Value)>,
}

impl<Key, Value> HashList<Key, Value>
where
    Key: PartialEq,
{
    pub fn new() -> Self {
        Self { data: vec![] }
    }
    pub fn contains_key(&self, k: &Key) -> bool {
        for (key, _) in &self.data {
            if key == k {
                return true;
            }
        }
        return false;
    }
    pub fn keys(&self) -> Vec<&Key> {
        let mut keys: Vec<&Key> = vec![];
        for (key, _) in &self.data {
            keys.push(key);
        }

        return keys;
    }
    pub fn insert(&mut self, k: Key, v: Value) -> bool {
        let mut contains_idx: Option<usize> = None;
        for (idx, (key, _)) in self.data.iter().enumerate() {
            if key == &k {
                contains_idx = Some(idx);
                break;
            }
        }
        match contains_idx {
            Some(idx) => {
                self.data.remove(idx);
            }
            None => {}
        }

        self.data.push((k, v));
        return contains_idx.is_some();
    }
    pub fn get(&self, k: &Key) -> Option<&Value> {
        for (idx, (key, value)) in self.data.iter().enumerate() {
            if key == k {
                return Some(value);
            }
        }
        return None;
    }
    pub fn get_mut(&mut self, k: &Key) -> Option<&mut Value> {
        for (idx, (key, value)) in self.data.iter_mut().enumerate() {
            if key == k {
                return Some(value);
            }
        }
        return None;
    }
}
