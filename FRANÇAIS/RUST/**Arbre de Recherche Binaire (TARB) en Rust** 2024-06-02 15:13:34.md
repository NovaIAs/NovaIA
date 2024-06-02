**Tri Arbre de Recherche Binaire (TARB) en Rust**

Un TARB est une structure de données arborescente qui stocke des éléments dans un ordre trié et permet des opérations de recherche, d'insertion et de suppression efficaces.

```rust
// Défini un nœud de TARB contenant une clé et une valeur
#[derive(Debug)]
struct Node<K, V> {
    key: K,
    value: V,
    left: Option<Box<Node<K, V>>>,
    right: Option<Box<Node<K, V>>>,
}

// Implémente le TARB
pub struct TARB<K: Ord, V> {
    root: Option<Box<Node<K, V>>>,
}

// Insère une clé-valeur dans le TARB
pub fn insert(&mut self, key: K, value: V) {
    match self.root.take() {
        None => self.root = Some(Box::new(Node { key, value, left: None, right: None })),
        Some(mut node) => {
            if key < node.key {
                if let Some(left) = node.left.take() {
                    left.insert(key, value);
                    node.left = Some(Box::new(left));
                } else {
                    node.left = Some(Box::new(Node { key, value, left: None, right: None }));
                }
            } else {
                if let Some(right) = node.right.take() {
                    right.insert(key, value);
                    node.right = Some(Box::new(right));
                } else {
                    node.right = Some(Box::new(Node { key, value, left: None, right: None }));
                }
            }
            self.root = Some(node);
        }
    }
}

// Recherche une clé dans le TARB et renvoie sa valeur
pub fn find(&self, key: &K) -> Option<&V> {
    match &self.root {
        None => None,
        Some(node) => node.find(key),
    }
}

impl<K: Ord, V> Node<K, V> {
    // Insère une clé-valeur dans le nœud
    pub fn insert(&mut self, key: K, value: V) {
        if key < self.key {
            if let Some(left) = self.left.take() {
                left.insert(key, value);
                self.left = Some(Box::new(left));
            } else {
                self.left = Some(Box::new(Node { key, value, left: None, right: None }));
            }
        } else {
            if let Some(right) = self.right.take() {
                right.insert(key, value);
                self.right = Some(Box::new(right));
            } else {
                self.right = Some(Box::new(Node { key, value, left: None, right: None }));
            }
        }
    }

    // Recherche une clé dans le nœud et renvoie sa valeur
    pub fn find(&self, key: &K) -> Option<&V> {
        if key == &self.key {
            Some(&self.value)
        } else if key < &self.key {
            self.left.as_ref().and_then(|left| left.find(key))
        } else {
            self.right.as_ref().and_then(|right| right.find(key))
        }
    }
}
```

**Utilisation**

```rust
// Créer un TARB
let mut tarb: TARB<i32, String> = TARB::new();

// Insérer des éléments
tarb.insert(1, "Un".to_string());
tarb.insert(3, "Trois".to_string());
tarb.insert(2, "Deux".to_string());

// Rechercher un élément
if let Some(value) = tarb.find(&2) {
    println!("Trouvé la valeur {} pour la clé 2", value);
}
```