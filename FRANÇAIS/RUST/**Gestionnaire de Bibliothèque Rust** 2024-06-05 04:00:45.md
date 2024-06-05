**Système de Gestion de Bibliothèque en Rust**

**Nom du fichier :** `bibliotheque.rs`

```rust
use std::collections::HashMap;

struct Livre {
    titre: String,
    auteur: String,
    annee_publication: i32,
    disponible: bool,
}

struct Bibliotheque {
    livres: HashMap<String, Livre>,
}

impl Bibliotheque {
    fn nouvelle_bibliotheque() -> Bibliotheque {
        Bibliotheque {
            livres: HashMap::new(),
        }
    }

    fn ajouter_livre(&mut self, titre: &str, auteur: &str, annee_publication: i32) {
        self.livres.insert(
            titre.to_string(),
            Livre {
                titre: titre.to_string(),
                auteur: auteur.to_string(),
                annee_publication: annee_publication,
                disponible: true,
            },
        );
    }

    fn emprunter_livre(&mut self, titre: &str) {
        if let Some(livre) = self.livres.get_mut(titre) {
            livre.disponible = false;
        }
    }

    fn rendre_livre(&mut self, titre: &str) {
        if let Some(livre) = self.livres.get_mut(titre) {
            livre.disponible = true;
        }
    }

    fn rechercher_livre(&self, titre: &str) -> Option<&Livre> {
        self.livres.get(titre)
    }

    fn lister_livres(&self) -> Vec<&Livre> {
        self.livres.values().collect()
    }

    fn lister_livres_disponibles(&self) -> Vec<&Livre> {
        self.livres
            .values()
            .filter(|livre| livre.disponible)
            .collect()
    }
}

fn main() {
    let mut bibliotheque = Bibliotheque::nouvelle_bibliotheque();

    // Ajouter quelques livres à la bibliothèque
    bibliotheque.ajouter_livre("Le Seigneur des Anneaux", "J.R.R. Tolkien", 1954);
    bibliotheque.ajouter_livre("1984", "George Orwell", 1949);
    bibliotheque.ajouter_livre("Le Petit Prince", "Antoine de Saint-Exupéry", 1943);

    // Emprunter le livre "Le Seigneur des Anneaux"
    bibliotheque.emprunter_livre("Le Seigneur des Anneaux");

    // Lister les livres disponibles
    let livres_disponibles = bibliotheque.lister_livres_disponibles();
    for livre in livres_disponibles {
        println!("Livre disponible : {:?}", livre);
    }

    // Rendre le livre "Le Seigneur des Anneaux"
    bibliotheque.rendre_livre("Le Seigneur des Anneaux");
}
```

**Explication du code :**

Ce code définit un système de gestion de bibliothèque en Rust, permettant d'ajouter, d'emprunter et de rendre des livres, ainsi que de lister les livres disponibles et la totalité des livres.

**Structure Livre :**
* Représente un livre avec des champs `titre`, `auteur`, `annee_publication` et `disponible`.

**Structure Bibliotheque :**
* Contient une collection de livres stockés dans une `HashMap` avec le titre comme clé.

**Méthodes de la structure Bibliotheque :**
* `nouvelle_bibliotheque()` : Crée une nouvelle instance de bibliothèque vide.
* `ajouter_livre()` : Ajoute un livre à la bibliothèque.
* `emprunter_livre()` : Marque un livre comme emprunté.
* `rendre_livre()` : Marque un livre comme rendu.
* `rechercher_livre()` : Recherche un livre par son titre.
* `lister_livres()` : Liste tous les livres de la bibliothèque.
* `lister_livres_disponibles()` : Liste uniquement les livres disponibles.

**Fonction main :**
* Crée une instance de bibliothèque et ajoute quelques livres.
* Emprunte le livre "Le Seigneur des Anneaux".
* Liste les livres disponibles.
* Rend le livre "Le Seigneur des Anneaux".