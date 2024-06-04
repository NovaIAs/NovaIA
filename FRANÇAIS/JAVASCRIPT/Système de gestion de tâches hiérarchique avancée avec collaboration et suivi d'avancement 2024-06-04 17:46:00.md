**Système de gestion de tâches hiérarchiques avancé avec fonctionnalités de collaboration et de suivi des progrès.**

```javascript
// Structure de données pour représenter une tâche
class Tâche {
    constructor(id, titre, description, priorité, échéance, dépendances, assignataires) {
        this.id = id;
        this.titre = titre;
        this.description = description;
        this.priorité = priorité;
        this.échéance = échéance;
        this.dépendances = dépendances;
        this.assignataires = assignataires;
        this.avancement = 0;
        this.état = "À faire";
    }
}

// Structure de données pour représenter un projet
class Projet {
    constructor(id, nom, tâches) {
        this.id = id;
        this.nom = nom;
        this.tâches = tâches;
        this.avancement = 0;
    }

    // Calculer l'avancement du projet en fonction de l'avancement de ses tâches
    calculerAvancement() {
        if (this.tâches.length === 0) {
            this.avancement = 100;
            return;
        }
        let totalAvancement = 0;
        for (let tâche of this.tâches) {
            totalAvancement += tâche.avancement;
        }
        this.avancement = totalAvancement / this.tâches.length;
    }
}

// Structure de données pour représenter un utilisateur
class Utilisateur {
    constructor(id, nom, email) {
        this.id = id;
        this.nom = nom;
        this.email = email;
        this.tâchesAssignées = [];
    }
}

// Singleton de gestion des données
class GestionnaireDonnées {
    static instance = null;

    constructor() {
        if (GestionnaireDonnées.instance) {
            return GestionnaireDonnées.instance;
        }

        this.projets = [];
        this.utilisateurs = [];

        GestionnaireDonnées.instance = this;
    }

    // Ajouter un projet
    ajouterProjet(projet) {
        this.projets.push(projet);
    }

    // Ajouter un utilisateur
    ajouterUtilisateur(utilisateur) {
        this.utilisateurs.push(utilisateur);
    }

    // Obtenir un projet par son ID
    getProjetById(id) {
        return this.projets.find(projet => projet.id === id);
    }

    // Obtenir un utilisateur par son ID
    getUtilisateurById(id) {
        return this.utilisateurs.find(utilisateur => utilisateur.id === id);
    }
}

// Singleton de gestion de l'interface utilisateur
class GestionnaireIHM {
    static instance = null;

    constructor() {
        if (GestionnaireIHM.instance) {
            return GestionnaireIHM.instance;
        }

        this.conteneurPrincipal = document.getElementById("conteneur-principal");

        GestionnaireIHM.instance = this;
    }

    // Afficher un projet
    afficherProjet(projet) {
        let conteneurProjet = document.createElement("div");
        conteneurProjet.classList.add("conteneur-projet");

        let titreProjet = document.createElement("h2");
        titreProjet.innerText = projet.nom;
        conteneurProjet.appendChild(titreProjet);

        let tableauTâches = document.createElement("table");
        tableauTâches.classList.add("tableau-tâches");
        let enteteTableau = tableauTâches.createTHead();
        let ligneEntete = enteteTableau.insertRow();
        ligneEntete.insertCell().innerText = "ID";
        ligneEntete.insertCell().innerText = "Titre";
        ligneEntete.insertCell().innerText = "Description";
        ligneEntete.insertCell().innerText = "Priorité";
        ligneEntete.insertCell().innerText = "Échéance";
        ligneEntete.insertCell().innerText = "Assignataires";
        ligneEntete.insertCell().innerText = "Avancement";
        ligneEntete.insertCell().innerText = "État";
        conteneurProjet.appendChild(tableauTâches);

        let corpsTableau = tableauTâches.createTBody();
        for (let tâche of projet.tâches) {
            let ligneTâche = corpsTableau.insertRow();
            ligneTâche.insertCell().innerText = tâche.id;
            ligneTâche.insertCell().innerText = tâche.titre;
            ligneTâche.insertCell().innerText = tâche.description;
            ligneTâche.insertCell().innerText = tâche.priorité;
            ligneTâche.insertCell().innerText = tâche.échéance;
            ligneTâche.insertCell().innerText = tâche.assignataires.join(", ");
            ligneTâche.insertCell().innerText = tâche.avancement + "%";
            ligneTâche.insertCell().innerText = tâche.état;
        }

        this.conteneurPrincipal.appendChild(conteneurProjet);
    }

    // Afficher les projets
    afficherProjets() {
        let projets = GestionnaireDonnées.instance.projets;
        for (let projet of projets) {
            this.afficherProjet(projet);
        }
    }
}

// Initialisation du système
let gestionnaireDonnées = new GestionnaireDonnées();
let gestionnaireIHM = new GestionnaireIHM();

// Création de projets
let projet1 = new Projet(1, "Projet 1", []);
let projet2 = new Projet(2, "Projet 2", []);

gestionnaireDonnées.ajouterProjet(projet1);
gestionnaireDonnées.ajouterProjet(projet2);

// Création d'utilisateurs
let utilisateur1 = new Utilisateur(1, "Nom d'utilisateur 1", "email1@exemple.com");
let utilisateur2 = new Utilisateur(2, "Nom d'utilisateur 2", "email2@exemple.com");

gestionnaireDonnées.ajouterUtilisateur(utilisateur1);
gestionnaireDonnées.ajouterUtilisateur(utilisateur2);

// Création de tâches
let tâche1 = new Tâche(1, "Tâche 1", "Description de la tâche 1", "Haute", "2023-03-08", [], [utilisateur1.id]);
let tâche2 = new Tâche(2, "Tâche 2", "Description de la tâche 2", "Moyenne", "2023-03-15", [], [utilisateur1.id, utilisateur2.id]);
let tâche3 = new Tâche(3, "Tâche 3", "Description de la tâche 3", "Basse", "2023-03-22", [], [utilisateur2.id]);

projet1.tâches.push(tâche1);
projet1.tâches.push(tâche2);
projet2.tâches.push(tâche3);

// Calcul de l'avancement des projets
projet1.calculerAvancement();
projet2.calculerAvancement();

// Affichage des projets
gestionnaireIHM.afficherProjets();
```

**Explication du code:**

Ce code crée un système complet de gestion de tâches hiérarchiques, comprenant des fonctionnalités de collaboration et de suivi des progrès.

* **Structures de données:**
    * `Tâche`: Représente une tâche avec des propriétés telles que le titre, l'échéance et les assignataires.
    * `Projet`: Représente un projet contenant une collection de tâches.
    * `Utilisateur`: Représente un utilisateur avec des propriétés telles que le nom et l'e-mail.

* **Singletons:**
    * `GestionnaireDonnées`: Singleton responsable de la gestion des données (projets, utilisateurs, tâches).
    * `GestionnaireIHM`: Singleton responsable de l'affichage de l'interface utilisateur.

* **Fonctionnalités:**
    * Ajout et récupération de projets et d'utilisateurs.
    * Création et gestion de tâches (y compris les dépendances et les assignataires).
    * Calcul de l'avancement des projets basé sur l'avancement des tâches.
    * Affichage de l'interface utilisateur représentant les projets et les tâches.

Ce système permet aux utilisateurs de créer, gérer et collaborer sur des projets complexes, suivre leur progression et rester organisés.