**Système de Gestion de Bibliothèque Complexe en Rust**

```rust
// Import des bibliothèques nécessaires
use std::io;
use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;

// Structure "Livre" représentant un livre dans la bibliothèque
#[derive(Debug)]
struct Livre {
    titre: String,
    auteur: String,
    isbn: String,
    exemplaires_disponibles: usize,
    exemplaires_empruntés: usize,
}

// Structure "Abonnement" représentant un abonné à la bibliothèque
#[derive(Debug)]
struct Abonnement {
    nom: String,
    prenom: String,
    numero_carte: String,
    livres_empruntés: HashSet<String>, // ISBN des livres empruntés
}

// Structure "Bibliothèque" représentant la bibliothèque
#[derive(Debug)]
struct Bibliothèque {
    livres: HashMap<String, Livre>, // ISBN -> Livre
    abonnés: HashMap<String, Abonnement>, // Numéro de carte -> Abonnement
}

// Implémentation de la fonction "ajouter_livre" pour ajouter un livre à la bibliothèque
impl Bibliothèque {
    fn ajouter_livre(&mut self, livre: Livre) {
        let isbn = &livre.isbn;
        self.livres.insert(isbn.clone(), livre);
    }
}

// Implémentation de la fonction "ajouter_abonné" pour ajouter un abonné à la bibliothèque
impl Bibliothèque {
    fn ajouter_abonné(&mut self, abonnement: Abonnement) {
        let numero_carte = &abonnement.numero_carte;
        self.abonnés.insert(numero_carte.clone(), abonnement);
    }
}

// Implémentation de la fonction "emprunter_livre" pour emprunter un livre
impl Bibliothèque {
    fn emprunter_livre(&mut self, isbn: &str, numero_carte: &str) -> Result<(), String> {
        // Récupérer le livre et l'abonné
        let livre = self.livres.get_mut(isbn).ok_or("Livre introuvable")?;
        let abonnement = self.abonnés.get_mut(numero_carte).ok_or("Abonnement introuvable")?;

        // Vérifier la disponibilité du livre
        if livre.exemplaires_disponibles == 0 {
            return Err("Le livre n'est pas disponible pour le moment".to_string());
        }

        // Emprunter le livre
        livre.exemplaires_disponibles -= 1;
        abonnement.livres_empruntés.insert(isbn.to_string());
        Ok(())
    }
}

// Implémentation de la fonction "retourner_livre" pour retourner un livre
impl Bibliothèque {
    fn retourner_livre(&mut self, isbn: &str, numero_carte: &str) -> Result<(), String> {
        // Récupérer le livre et l'abonné
        let livre = self.livres.get_mut(isbn).ok_or("Livre introuvable")?;
        let abonnement = self.abonnés.get_mut(numero_carte).ok_or("Abonnement introuvable")?;

        // Vérifier si l'abonné a emprunté le livre
        if !abonnement.livres_empruntés.contains(isbn) {
            return Err("L'abonné n'a pas emprunté ce livre".to_string());
        }

        // Retourner le livre
        livre.exemplaires_disponibles += 1;
        abonnement.livres_empruntés.remove(isbn);
        Ok(())
    }
}

// Implémentation de la fonction "afficher_catalog" pour afficher le catalogue des livres
impl Bibliothèque {
    fn afficher_catalog(&self) {
        println!("Catalogue de la bibliothèque:");
        for (isbn, livre) in &self.livres {
            println!(" - {} - {} par {}", isbn, livre.titre, livre.auteur);
        }
    }
}

// Implémentation de la fonction "afficher_abonnés" pour afficher la liste des abonnés
impl Bibliothèque {
    fn afficher_abonnés(&self) {
        println!("Liste des abonnés:");
        for (numero_carte, abonnement) in &self.abonnés {
            println!(" - {} - {} {}", numero_carte, abonnement.nom, abonnement.prenom);
        }
    }
}

// Fonction principale "main"
fn main() {
    // Créer une nouvelle bibliothèque
    let mut bibliotheque = Bibliothèque {
        livres: HashMap::new(),
        abonnés: HashMap::new(),
    };

    // Ajouter des livres à la bibliothèque
    bibliotheque.ajouter_livre(Livre {
        titre: "Le Petit Prince".to_string(),
        auteur: "Antoine de Saint-Exupéry".to_string(),
        isbn: "123456789".to_string(),
        exemplaires_disponibles: 5,
        exemplaires_empruntés: 0,
    });
    bibliotheque.ajouter_livre(Livre {
        titre: "1984".to_string(),
        auteur: "George Orwell".to_string(),
        isbn: "987654321".to_string(),
        exemplaires_disponibles: 3,
        exemplaires_empruntés: 0,
    });

    // Ajouter des abonnés à la bibliothèque
    bibliotheque.ajouter_abonné(Abonnement {
        nom: "Dupont".to_string(),
        prenom: "Jean".to_string(),
        numero_carte: "123".to_string(),
        livres_empruntés: HashSet::new(),
    });
    bibliotheque.ajouter_abonné(Abonnement {
        nom: "Durand".to_string(),
        prenom: "Marie".to_string(),
        numero_carte: "456".to_string(),
        livres_empruntés: HashSet::new(),
    });

    // Emprunter un livre
    match bibliotheque.emprunter_livre("123456789", "123") {
        Ok(()) => println!("Emprunt réussi"),
        Err(e) => println!("Erreur d'emprunt : {}", e),
    }

    // Retourner un livre
    match bibliotheque.retourner_livre("123456789", "123") {
        Ok(()) => println!("Retour réussi"),
        Err(e) => println!("Erreur de retour : {}", e),
    }

    // Afficher le catalogue des livres
    bibliotheque.afficher_catalog();

    // Afficher la liste des abonnés
    bibliotheque.afficher_abonnés();
}
```

**Explication du code:**

Ce code implémente un système de gestion de bibliothèque complet en Rust. Il utilise des structures de données telles que des HashMaps et des HashSets pour stocker des informations sur les livres et les abonnés. Voici une brève explication de chaque composante du code:

* **Structures:**
    * `Livre`: Représente un livre avec son titre, son auteur, son ISBN, le nombre d'exemplaires disponibles et le nombre d'exemplaires empruntés.
    * `Abonnement`: Représente un abonné avec son nom, son prénom, son numéro de carte et la liste des livres empruntés.
    * `Bibliothèque`: Représente la bibliothèque, contenant des HashMaps de livres et d'abonnés.
* **Méthodes d'instance:**
    * `ajouter_livre`: Ajoute un livre à la bibliothèque.
    * `ajouter_abonné`: Ajoute un abonné à la bibliothèque.
    * `emprunter_livre`: Permet d'emprunter un livre si disponible et met à jour l'abonné.
    * `retourner_livre`: Permet de retourner un livre et met à jour l'abonné.
    * `afficher_catalog`: Affiche le catalogue des livres.
    * `afficher_abonnés`: Affiche la liste des abonnés.
* **Fonction principale:**
    * Crée une bibliothèque, ajoute des livres et des abonnés, effectue des opérations d'emprunt et de retour, et affiche le catalogue des livres et la liste des abonnés.

Ce code fournit un système de gestion de bibliothèque fonctionnel avec des fonctionnalités essentielles telles que le suivi des livres, des abonnés, des emprunts et des retours.