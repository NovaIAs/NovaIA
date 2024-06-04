**Système de gestion de bibliothèque**

```javascript
// Modèle de données des livres
class Livre {
    constructor(id, titre, auteur, categorie, isbn, quantite) {
        this.id = id;
        this.titre = titre;
        this.auteur = auteur;
        this.categorie = categorie;
        this.isbn = isbn;
        this.quantite = quantite;
    }
}

// Modèle de données des utilisateurs
class Utilisateur {
    constructor(id, nom, email, type) {
        this.id = id;
        this.nom = nom;
        this.email = email;
        this.type = type; // Étudiant, Enseignant, Administrateur
    }
}

// Service de gestion des livres
class ServiceLivres {
    constructor() {
        this.livres = [];
    }

    ajouterLivre(livre) {
        this.livres.push(livre);
    }

    supprimerLivre(id) {
        const index = this.livres.findIndex((l) => l.id === id);
        if (index !== -1) {
            this.livres.splice(index, 1);
        }
    }

    modifierLivre(id, nouveauLivre) {
        const index = this.livres.findIndex((l) => l.id === id);
        if (index !== -1) {
            this.livres[index] = nouveauLivre;
        }
    }

    rechercherLivre(critere, valeur) {
        switch (critere) {
            case "id":
                return this.livres.find((l) => l.id === valeur);
            case "titre":
                return this.livres.filter((l) => l.titre.includes(valeur));
            case "auteur":
                return this.livres.filter((l) => l.auteur.includes(valeur));
            case "categorie":
                return this.livres.filter((l) => l.categorie === valeur);
            case "isbn":
                return this.livres.find((l) => l.isbn === valeur);
            default:
                return [];
        }
    }

    listerLivres() {
        return this.livres;
    }
}

// Service de gestion des utilisateurs
class ServiceUtilisateurs {
    constructor() {
        this.utilisateurs = [];
    }

    ajouterUtilisateur(utilisateur) {
        this.utilisateurs.push(utilisateur);
    }

    supprimerUtilisateur(id) {
        const index = this.utilisateurs.findIndex((u) => u.id === id);
        if (index !== -1) {
            this.utilisateurs.splice(index, 1);
        }
    }

    modifierUtilisateur(id, nouvelUtilisateur) {
        const index = this.utilisateurs.findIndex((u) => u.id === id);
        if (index !== -1) {
            this.utilisateurs[index] = nouvelUtilisateur;
        }
    }

    rechercherUtilisateur(critere, valeur) {
        switch (critere) {
            case "id":
                return this.utilisateurs.find((u) => u.id === valeur);
            case "nom":
                return this.utilisateurs.filter((u) => u.nom.includes(valeur));
            case "email":
                return this.utilisateurs.filter((u) => u.email === valeur);
            case "type":
                return this.utilisateurs.filter((u) => u.type === valeur);
            default:
                return [];
        }
    }

    listerUtilisateurs() {
        return this.utilisateurs;
    }
}

// Service de gestion des emprunts
class ServiceEmprunts {
    constructor() {
        this.emprunts = [];
    }

    ajouterEmprunt(emprunt) {
        this.emprunts.push(emprunt);
    }

    supprimerEmprunt(id) {
        const index = this.emprunts.findIndex((e) => e.id === id);
        if (index !== -1) {
            this.emprunts.splice(index, 1);
        }
    }

    modifierEmprunt(id, nouvelEmprunt) {
        const index = this.emprunts.findIndex((e) => e.id === id);
        if (index !== -1) {
            this.emprunts[index] = nouvelEmprunt;
        }
    }

    rechercherEmprunt(critere, valeur) {
        switch (critere) {
            case "id":
                return this.emprunts.find((e) => e.id === valeur);
            case "utilisateur":
                return this.emprunts.filter((e) => e.utilisateur === valeur);
            case "livre":
                return this.emprunts.filter((e) => e.livre === valeur);
            case "dateEmprunt":
                return this.emprunts.filter((e) => e.dateEmprunt === valeur);
            case "dateRetour":
                return this.emprunts.filter((e) => e.dateRetour === valeur);
            default:
                return [];
        }
    }

    listerEmprunts() {
        return this.emprunts;
    }
}

// Application principale
class App {
    constructor() {
        this.serviceLivres = new ServiceLivres();
        this.serviceUtilisateurs = new ServiceUtilisateurs();
        this.serviceEmprunts = new ServiceEmprunts();
    }

    initialiser() {
        // Chargement des données initiales
        const livres = [
            new Livre(1, "Le Petit Prince", "Antoine de Saint-Exupéry", "Enfants", "9782070580509", 10),
            new Livre(2, "1984", "George Orwell", "Fiction", "9780451524935", 5),
            new Livre(3, "Le Seigneur des anneaux", "J.R.R. Tolkien", "Fantasy", "9780395082560", 2)
        ];
        livres.forEach((l) => this.serviceLivres.ajouterLivre(l));

        const utilisateurs = [
            new Utilisateur(1, "Jean Dubois", "jean.dubois@gmail.com", "Étudiant"),
            new Utilisateur(2, "Marie Dupont", "marie.dupont@gmail.com", "Enseignant"),
            new Utilisateur(3, "Paul Martin", "paul.martin@gmail.com", "Administrateur")
        ];
        utilisateurs.forEach((u) => this.serviceUtilisateurs.ajouterUtilisateur(u));
    }

    // Méthodes d'affichage
    afficherLivres() {
        const livres = this.serviceLivres.listerLivres();
        console.table(livres);
    }

    afficherUtilisateurs() {
        const utilisateurs = this.serviceUtilisateurs.listerUtilisateurs();
        console.table(utilisateurs);
    }

    afficherEmprunts() {
        const emprunts = this.serviceEmprunts.listerEmprunts();
        console.table(emprunts);
    }

    // Méthodes de gestion
    ajouterLivre(livre) {
        this.serviceLivres.ajouterLivre(livre);
    }

    supprimerLivre(id) {
        this.serviceLivres.supprimerLivre(id);
    }

    modifierLivre(id, nouveauLivre) {
        this.serviceLivres.modifierLivre(id, nouveauLivre);
    }

    ajouterUtilisateur(utilisateur) {
        this.serviceUtilisateurs.ajouterUtilisateur(utilisateur);
    }

    supprimerUtilisateur(id) {
        this.serviceUtilisateurs.supprimerUtilisateur(id);
    }

    modifierUtilisateur(id, nouvelUtilisateur) {
        this.serviceUtilisateurs.modifierUtilisateur(id, nouvelUtilisateur);
    }

    ajouterEmprunt(emprunt) {
        this.serviceEmprunts.ajouterEmprunt(emprunt);
    }

    supprimerEmprunt(id) {
        this.serviceEmprunts.supprimerEmprunt(id);
    }

    modifierEmprunt(id, nouvelEmprunt) {
        this.serviceEmprunts.modifierEmprunt(id, nouvelEmprunt);
    }
}

// Exécution de l'application
const app = new App();
app.initialiser();
app.afficherLivres();
app.afficherUtilisateurs();
app.afficherEmprunts();
```

**Explication du code:**

Ce code JavaScript implémente un système de gestion de bibliothèque complet avec les fonctionnalités suivantes :

* **Gestion des livres:** Ajout, suppression, modification et recherche de livres par différents critères.
* **Gestion des utilisateurs:** Aj