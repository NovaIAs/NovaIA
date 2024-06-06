**Gestionnaire d'Inventaire Complexe en Rust**

**Objectif:**
Créer un système de gestion d'inventaire robuste et complet qui gère les produits, les emplacements et les transactions.

**Code:**

```rust
use std::{collections::HashMap, io::stdin};

fn main() {
    // Structures de données
    // ====================

    // Produit
    struct Produit {
        id: String,
        nom: String,
        quantite: i32,
        prix: f64,
    }

    // Emplacement
    struct Emplacement {
        id: String,
        nom: String,
        produits: HashMap<String, i32>,
    }

    // Transaction
    struct Transaction {
        id: String,
        date: String,
        type_transaction: String, // "Entrée" ou "Sortie"
        emplacement_id: String,
        produit_id: String,
        quantite: i32,
    }

    // Données d'inventaire
    // ====================

    // Produits
    let mut produits: HashMap<String, Produit> = HashMap::new();
    produits.insert(
        "P1".to_string(),
        Produit {
            id: "P1".to_string(),
            nom: "Produit 1".to_string(),
            quantite: 50,
            prix: 10.0,
        },
    );

    // Emplacements
    let mut emplacements: HashMap<String, Emplacement> = HashMap::new();
    emplacements.insert(
        "E1".to_string(),
        Emplacement {
            id: "E1".to_string(),
            nom: "Entrepôt 1".to_string(),
            produits: HashMap::new(),
        },
    );

    // Transactions
    let mut transactions: Vec<Transaction> = Vec::new();

    // Boucle d'interaction
    // ====================

    loop {
        println!("Choisissez une action :");
        println!("1. Ajouter un produit");
        println!("2. Ajouter un emplacement");
        println!("3. Enregistrer une transaction");
        println!("4. Afficher l'inventaire");
        println!("5. Quitter");

        let mut choix = String::new();
        stdin().read_line(&mut choix).unwrap();

        match choix.trim() {
            "1" => ajouter_produit(&mut produits),
            "2" => ajouter_emplacement(&mut emplacements),
            "3" => enregistrer_transaction(&mut transactions, &mut produits, &mut emplacements),
            "4" => afficher_inventaire(&produits, &emplacements),
            "5" => break,
            _ => println!("Choix invalide !"),
        }
    }
}

// Fonctions auxiliaires
// ====================

fn ajouter_produit(produits: &mut HashMap<String, Produit>) {
    let mut id = String::new();
    let mut nom = String::new();
    let mut quantite = String::new();
    let mut prix = String::new();

    println!("Entrez l'ID du produit :");
    stdin().read_line(&mut id).unwrap();
    println!("Entrez le nom du produit :");
    stdin().read_line(&mut nom).unwrap();
    println!("Entrez la quantité :");
    stdin().read_line(&mut quantite).unwrap();
    println!("Entrez le prix :");
    stdin().read_line(&mut prix).unwrap();

    let produit = Produit {
        id: id.trim().to_string(),
        nom: nom.trim().to_string(),
        quantite: quantite.trim().parse().unwrap(),
        prix: prix.trim().parse().unwrap(),
    };

    produits.insert(id.trim().to_string(), produit);
    println!("Produit ajouté avec succès !");
}

fn ajouter_emplacement(emplacements: &mut HashMap<String, Emplacement>) {
    let mut id = String::new();
    let mut nom = String::new();

    println!("Entrez l'ID de l'emplacement :");
    stdin().read_line(&mut id).unwrap();
    println!("Entrez le nom de l'emplacement :");
    stdin().read_line(&mut nom).unwrap();

    let emplacement = Emplacement {
        id: id.trim().to_string(),
        nom: nom.trim().to_string(),
        produits: HashMap::new(),
    };

    emplacements.insert(id.trim().to_string(), emplacement);
    println!("Emplacement ajouté avec succès !");
}

fn enregistrer_transaction(
    transactions: &mut Vec<Transaction>,
    produits: &mut HashMap<String, Produit>,
    emplacements: &mut HashMap<String, Emplacement>,
) {
    let mut id = String::new();
    let mut date = String::new();
    let mut type_transaction = String::new();
    let mut emplacement_id = String::new();
    let mut produit_id = String::new();
    let mut quantite = String::new();

    println!("Entrez l'ID de la transaction :");
    stdin().read_line(&mut id).unwrap();
    println!("Entrez la date de la transaction :");
    stdin().read_line(&mut date).unwrap();
    println!("Entrez le type de transaction (Entrée/Sortie) :");
    stdin().read_line(&mut type_transaction).unwrap();
    println!("Entrez l'ID de l'emplacement :");
    stdin().read_line(&mut emplacement_id).unwrap();
    println!("Entrez l'ID du produit :");
    stdin().read_line(&mut produit_id).unwrap();
    println!("Entrez la quantité :");
    stdin().read_line(&mut quantite).unwrap();

    let transaction = Transaction {
        id: id.trim().to_string(),
        date: date.trim().to_string(),
        type_transaction: type_transaction.trim().to_string(),
        emplacement_id: emplacement_id.trim().to_string(),
        produit_id: produit_id.trim().to_string(),
        quantite: quantite.trim().parse().unwrap(),
    };

    match type_transaction.trim() {
        "Entrée" => {
            produits.get_mut(&produit_id).unwrap().quantite += quantite.trim().parse().unwrap();
            emplacements
                .get_mut(&emplacement_id)
                .unwrap()
                .produits
                .insert(produit_id.trim().to_string(), quantite.trim().parse().unwrap());
        }
        "Sortie" => {
            produits.get_mut(&produit_id).unwrap().quantite -= quantite.trim().parse().unwrap();
            emplacements
                .get_mut(&emplacement_id)
                .unwrap()
                .produits
                .remove(&produit_id.trim().to_string());
        }
        _ => println!("Type de transaction invalide !"),
    }

    transactions.push(transaction);
    println!("Transaction enregistrée avec succès !");
}

fn afficher_inventaire(produits: &HashMap<String, Produit>, emplacements: &HashMap<String, Emplacement>) {
    println!("**Produits :**");
    for produit in produits.values() {
        println!(" - ID : {} | Nom : {} | Quantité : {} | Prix : {}", produit.id, produit.nom, produit.quantite, produit.prix);
    }

    println!("**Emplacements :**");
    for emplacement in emplacements.values() {
        println!(" - ID : {} | Nom : {} | Produits :", emplacement.id, emplacement.nom);
        for produit_id in emplacement.produits.keys() {
            println!("  * {} : {}", produit_id, emplacement.produits[produit_id]);
        }
    }
}
```

**Explication du Code:**

* **Structures de Données:**
    * **Produit:** Représente un produit avec ses informations de base.
    * **Emplacement:** Représente un emplacement de stockage avec son nom et une liste de produits stockés.
    * **Transaction:** Représente une transaction d'entrée ou de sortie de produit.

* **Données d'Inventaire:**
    * Des HashMaps sont utilisées pour stocker des produits, des emplacements et des transactions.
    * Les données d'inventaire initiales sont fournies pour les exemples.

* **Boucle d'Interaction:**
    * Une boucle d'interaction permet aux utilisateurs de choisir des actions telles que l'ajout de produits, l'ajout d'emplacements, l'enregistrement de transactions et l'affichage de l'inventaire.

* **Fonctions Auxiliaires:**
    * Ces fonctions gèrent les interactions utilisateur, telles que l'ajout de produits, d'emplacements ou l'enregistrement de transactions.
    * Elles mettent à