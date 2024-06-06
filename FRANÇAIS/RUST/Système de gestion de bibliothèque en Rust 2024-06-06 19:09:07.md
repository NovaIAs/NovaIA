**Système de gestion de bibliothèque en Rust**

Ce système de gestion de bibliothèque permet de gérer les livres, les emprunts et les utilisateurs.

```rust
// Dépendances
use std::collections::{HashMap, VecDeque};
use std::io::{stdin, stdout, Write};
use std::fmt;

// Structure représentant un livre
struct Livre {
    titre: String,
    auteur: String,
    isbn: u32,
    exemplaires_disponibles: usize,
}

impl Livre {
    fn nouveau(titre: &str, auteur: &str, isbn: u32, exemplaires: usize) -> Livre {
        Livre {
            titre: titre.to_string(),
            auteur: auteur.to_string(),
            isbn,
            exemplaires_disponibles: exemplaires,
        }
    }
}

// Structure représentant un utilisateur
struct Utilisateur {
    nom: String,
    prenom: String,
    id: u32,
    livres_empruntés: VecDeque<u32>,
}

impl Utilisateur {
    fn nouveau(nom: &str, prenom: &str, id: u32) -> Utilisateur {
        Utilisateur {
            nom: nom.to_string(),
            prenom: prenom.to_string(),
            id,
            livres_empruntés: VecDeque::new(),
        }
    }
}

// Structure représentant la bibliothèque
struct Bibliotheque {
    livres: HashMap<u32, Livre>,
    utilisateurs: HashMap<u32, Utilisateur>,
}

impl Bibliotheque {
    fn nouvelle() -> Bibliotheque {
        Bibliotheque {
            livres: HashMap::new(),
            utilisateurs: HashMap::new(),
        }
    }

    fn ajouter_livre(&mut self, livre: Livre) {
        self.livres.insert(livre.isbn, livre);
    }

    fn ajouter_utilisateur(&mut self, utilisateur: Utilisateur) {
        self.utilisateurs.insert(utilisateur.id, utilisateur);
    }

    fn emprunter_livre(&mut self, isbn: u32, id_utilisateur: u32) {
        // Vérifier si le livre est disponible
        if let Some(livre) = self.livres.get(&isbn) {
            if livre.exemplaires_disponibles > 0 {
                // Emprunter le livre
                livre.exemplaires_disponibles -= 1;

                // Ajouter le livre à la liste des livres empruntés par l'utilisateur
                if let Some(utilisateur) = self.utilisateurs.get_mut(&id_utilisateur) {
                    utilisateur.livres_empruntés.push_back(isbn);
                }
            }
        }
    }

    fn rendre_livre(&mut self, isbn: u32, id_utilisateur: u32) {
        // Vérifier si le livre est emprunté par l'utilisateur
        if let Some(utilisateur) = self.utilisateurs.get_mut(&id_utilisateur) {
            if let Some(index) = utilisateur.livres_empruntés.iter().position(|&x| x == isbn) {
                // Rendre le livre
                if let Some(livre) = self.livres.get_mut(&isbn) {
                    livre.exemplaires_disponibles += 1;
                }

                // Supprimer le livre de la liste des livres empruntés par l'utilisateur
                utilisateur.livres_empruntés.remove(index);
            }
        }
    }
}

// Menu principal
fn menu_principal(bibliotheque: &mut Bibliotheque) {
    loop {
        // Effacer l'écran
        print!("\x1B[2J");

        // Afficher le menu
        println!("**Gestion de bibliothèque**");
        println!("1. Ajouter un livre");
        println!("2. Ajouter un utilisateur");
        println!("3. Emprunter un livre");
        println!("4. Rendre un livre");
        println!("5. Quitter");

        // Lire la sélection utilisateur
        let mut choix = String::new();
        stdin().read_line(&mut choix).unwrap();

        // Traiter la sélection utilisateur
        match choix.trim() {
            "1" => ajouter_livre(bibliotheque),
            "2" => ajouter_utilisateur(bibliotheque),
            "3" => emprunter_livre(bibliotheque),
            "4" => rendre_livre(bibliotheque),
            "5" => break,
            _ => println!("Sélection invalide"),
        }

        // Attendre l'entrée utilisateur
        let mut pause = String::new();
        print!("Appuyez sur Entrée pour continuer...");
        stdout().flush().unwrap();
        stdin().read_line(&mut pause).unwrap();
    }
}

// Ajouter un livre
fn ajouter_livre(bibliotheque: &mut Bibliotheque) {
    // Lire les informations du livre
    println!("Titre :");
    let mut titre = String::new();
    stdin().read_line(&mut titre).unwrap();

    println!("Auteur :");
    let mut auteur = String::new();
    stdin().read_line(&mut auteur).unwrap();

    println!("ISBN :");
    let mut isbn = String::new();
    stdin().read_line(&mut isbn).unwrap();

    println!("Nombre d'exemplaires :");
    let mut exemplaires = String::new();
    stdin().read_line(&mut exemplaires).unwrap();

    // Créer le livre
    let livre = Livre::nouveau(
        titre.trim(),
        auteur.trim(),
        isbn.trim().parse().unwrap(),
        exemplaires.trim().parse().unwrap(),
    );

    // Ajouter le livre à la bibliothèque
    bibliotheque.ajouter_livre(livre);
}

// Ajouter un utilisateur
fn ajouter_utilisateur(bibliotheque: &mut Bibliotheque) {
    // Lire les informations de l'utilisateur
    println!("Nom :");
    let mut nom = String::new();
    stdin().read_line(&mut nom).unwrap();

    println!("Prénom :");
    let mut prenom = String::new();
    stdin().read_line(&mut prenom).unwrap();

    // Créer l'utilisateur
    let utilisateur = Utilisateur::nouveau(nom.trim(), prenom.trim(), bibliotheque.utilisateurs.len() as u32 + 1);

    // Ajouter l'utilisateur à la bibliothèque
    bibliotheque.ajouter_utilisateur(utilisateur);
}

// Emprunter un livre
fn emprunter_livre(bibliotheque: &mut Bibliotheque) {
    // Lire l'ISBN du livre
    println!("ISBN du livre à emprunter :");
    let mut isbn = String::new();
    stdin().read_line(&mut isbn).unwrap();

    // Lire l'ID de l'utilisateur
    println!("ID de l'utilisateur qui emprunte le livre :");
    let mut id_utilisateur = String::new();
    stdin().read_line(&mut id_utilisateur).unwrap();

    // Emprunter le livre
    bibliotheque.emprunter_livre(isbn.trim().parse().unwrap(), id_utilisateur.trim().parse().unwrap());
}

// Rendre un livre
fn rendre_livre(bibliotheque: &mut Bibliotheque) {
    // Lire l'ISBN du livre
    println!("ISBN du livre à rendre :");
    let mut isbn = String::new();
    stdin().read_line(&mut isbn).unwrap();

    // Lire l'ID de l'utilisateur
    println!("ID de l'utilisateur qui rend le livre :");
    let mut id_utilisateur = String::new();
    stdin().read_line(&mut id_utilisateur).unwrap();

    // Rendre le livre
    bibliotheque.rendre_livre(isbn.trim().parse().unwrap(), id_utilisateur.trim().parse().unwrap());
}

// Afficher la bibliothèque
impl fmt::Display for Bibliotheque {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Afficher les livres
        println!("**Livres**");
        for livre in self.livres.values() {
            println!("{livre}");
        }

        // Afficher les utilisateurs
        println!("**Utilisateurs**");
        for utilisateur in self.utilisateurs.values() {
            println!("{utilisateur}");
        }

        Ok(())
    }
}

fn main() {
    // Créer une bibliothèque
    let mut bibliotheque = Bibliotheque::nouvelle();

    // Ajouter quelques livres et utilisateurs
    bibliotheque.ajouter_livre(Livre::nouveau("Le Petit Prince", "Antoine de Saint-Exupéry", 0x123456789, 3));
    bibliotheque.ajouter_livre(Livre::nouveau("1984", "George Orwell", 0x987654321, 2));
    