use std::collections::HashMap;
use std::hash::Hash;

pub struct SymbolTable<K, V> {
    maps: Vec<HashMap<K, V>>,
}

impl<K, V> SymbolTable<K, V> {
    pub fn new() -> SymbolTable<K, V> {
        SymbolTable {
            maps: vec![HashMap::new()],
        }
    }

    pub fn get(&self, k: &K) -> Option<&V>
    where
        K: Hash + Eq,
    {
        for map in self.maps.iter().rev() {
            if let Some(v) = map.get(k) {
                return Some(v);
            }
        }
        None
    }

    pub fn get_mut(&mut self, k: &K) -> Option<&mut V>
    where
        K: Hash + Eq,
    {
        for map in self.maps.iter_mut().rev() {
            if let Some(v) = map.get_mut(k) {
                return Some(v);
            }
        }
        None
    }

    pub fn insert(&mut self, k: K, v: V)
    where
        K: Hash + Eq,
    {
        self.maps.last_mut().unwrap().insert(k, v);
    }

    pub fn enter_scope(&mut self) {
        self.maps.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        if self.maps.len() > 1 {
            self.maps.pop();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::SymbolTable;

    #[test]
    fn test_symbol_table() {
        let mut table = SymbolTable::new();
        table.insert("k1", "v1");
        table.insert("k2", "v2");
        assert_eq!(table.get(&"k1"), Some(&"v1"));
        assert_eq!(table.get(&"k2"), Some(&"v2"));
        table.enter_scope();
        table.insert("k2", "v3");
        assert_eq!(table.get(&"k1"), Some(&"v1"));
        assert_eq!(table.get(&"k2"), Some(&"v3"));
        assert_eq!(table.get(&"k3"), None);
        table.exit_scope();
        assert_eq!(table.get(&"k1"), Some(&"v1"));
        assert_eq!(table.get(&"k2"), Some(&"v2"));
    }
}
