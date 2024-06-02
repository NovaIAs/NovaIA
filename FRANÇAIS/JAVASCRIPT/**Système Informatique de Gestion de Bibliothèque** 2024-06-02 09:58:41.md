**Système de Gestion de Base de Données de Bibliothèque**

```javascript
// Déclarer l'objet base de données de la bibliothèque
const baseDeDonneesBibliotheque = {
  // Dictionnaire pour stocker les livres par leur identifiant
  livres: {},

  // Dictionnaire pour stocker les emprunteurs par leur identifiant
  emprunteurs: {},

  // Ajouter un livre à la base de données
  ajouterLivre: function(id, titre, auteur, genre) {
    this.livres[id] = {
      titre: titre,
      auteur: auteur,
      genre: genre,
      disponible: true,
      empruntePar: null
    };
  },

  // Supprimer un livre de la base de données
  supprimerLivre: function(id) {
    delete this.livres[id];
  },

  // Consulter les informations d'un livre
  consulterLivre: function(id) {
    return this.livres[id];
  },

  // Ajouter un emprunteur à la base de données
  ajouterEmprunteur: function(id, nom, adresse, email) {
    this.emprunteurs[id] = {
      nom: nom,
      adresse: adresse,
      email: email,
      livresEmpruntes: []
    };
  },

  // Supprimer un emprunteur de la base de données
  supprimerEmprunteur: function(id) {
    delete this.emprunteurs[id];
  },

  // Consulter les informations d'un emprunteur
  consulterEmprunteur: function(id) {
    return this.emprunteurs[id];
  },

  // Emprunter un livre par un emprunteur
  emprunterLivre: function(idLivre, idEmprunteur) {
    const livre = this.livres[idLivre];
    const emprunteur = this.emprunteurs[idEmprunteur];

    if (livre.disponible) {
      livre.disponible = false;
      livre.empruntePar = emprunteur.id;
      emprunteur.livresEmpruntes.push(idLivre);
    }
  },

  // Rendre un livre par un emprunteur
  rendreLivre: function(idLivre, idEmprunteur) {
    const livre = this.livres[idLivre];
    const emprunteur = this.emprunteurs[idEmprunteur];

    if (!livre.disponible) {
      livre.disponible = true;
      livre.empruntePar = null;
      emprunteur.livresEmpruntes = emprunteur.livresEmpruntes.filter(id => id !== idLivre);
    }
  },

  // Lister tous les livres disponibles
  listerLivresDisponibles: function() {
    return Object.values(this.livres).filter(livre => livre.disponible);
  },

  // Lister tous les emprunteurs avec leurs livres empruntés
  listerEmprunteursAvecLivres: function() {
    const emprunteursAvecLivres = [];

    for (const idEmprunteur in this.emprunteurs) {
      const emprunteur = this.emprunteurs[idEmprunteur];
      emprunteursAvecLivres.push({
        nom: emprunteur.nom,
        livresEmpruntes: emprunteur.livresEmpruntes.map(idLivre => this.livres[idLivre])
      });
    }

    return emprunteursAvecLivres;
  }
};

// Initialiser la base de données avec quelques livres et emprunteurs
baseDeDonneesBibliotheque.ajouterLivre(1, "Harry Potter et la Pierre Philosophale", "J.K. Rowling", "Fantastique");
baseDeDonneesBibliotheque.ajouterLivre(2, "Le Seigneur des Anneaux", "J.R.R. Tolkien", "Fantasy");
baseDeDonneesBibliotheque.ajouterLivre(3, "Le Petit Prince", "Antoine de Saint-Exupéry", "Enfants");
baseDeDonneesBibliotheque.ajouterLivre(4, "La Divine Comédie", "Dante Alighieri", "Classique");
baseDeDonneesBibliotheque.ajouterLivre(5, "1984", "George Orwell", "Science-Fiction");

baseDeDonneesBibliotheque.ajouterEmprunteur(10, "Alice", "123 Main Street", "alice@example.com");
baseDeDonneesBibliotheque.ajouterEmprunteur(11, "Bob", "456 Oak Street", "bob@example.com");
baseDeDonneesBibliotheque.ajouterEmprunteur(12, "Carol", "789 Maple Street", "carol@example.com");

// Emprunter un livre
baseDeDonneesBibliotheque.emprunterLivre(1, 10);

// Lister tous les livres disponibles
console.log("Livres disponibles :");
const livresDisponibles = baseDeDonneesBibliotheque.listerLivresDisponibles();
for (const livre of livresDisponibles) {
  console.log(livre.titre);
}

// Lister tous les emprunteurs avec leurs livres empruntés
console.log("\nEmprunteurs avec leurs livres empruntés :");
const emprunteursAvecLivres = baseDeDonneesBibliotheque.listerEmprunteursAvecLivres();
for (const emprunteur of emprunteursAvecLivres) {
  console.log(`${emprunteur.nom} :`);
  for (const livre of emprunteur.livresEmpruntes) {
    console.log(` - ${livre.titre}`);
  }
}
```

**Explication du code**

Ce code implémente un système de gestion de base de données pour une bibliothèque. Il dispose de fonctionnalités pour ajouter, supprimer, consulter et gérer les livres et les emprunteurs, ainsi que pour emprunter et rendre des livres.

* **Dictionnaires** : Le code utilise des dictionnaires (objets) pour stocker les livres et les emprunteurs. Chaque dictionnaire a une clé unique qui identifie le livre ou l'emprunteur.

* **Ajout et suppression** : Des méthodes sont définies pour ajouter et supprimer des livres et des emprunteurs de la base de données.

* **Consultation** : Des méthodes sont définies pour consulter les informations d'un livre ou d'un emprunteur particulier.

* **Emprunt et retour** : Des méthodes sont définies pour emprunter et rendre des livres. Ces méthodes mettent à jour la disponibilité du livre et la liste des livres empruntés par l'emprunteur.

* **Fonctions de liste** : Des méthodes sont définies pour lister tous les livres disponibles et tous les emprunteurs avec leurs livres empruntés.

Le code utilise des fonctions auxiliaires et des boucles pour effectuer les opérations nécessaires et fournir les résultats souhaités. Il est conçu pour être modulaire et extensible, permettant d'ajouter facilement de nouvelles fonctionnalités à l'avenir.