```typescript
interface Personne {
    nom: string;
    prénom: string;
    âge: number;
    adresses: Array<Adresse>;
}

interface Adresse {
    rue: string;
    codePostal: number;
    ville: string;
}

type StatutMarital = "Célibataire" | "Marié" | "Divorcé" | "Veuf";

class GestionnairePersonnes {

    private personnes: Array<Personne>;

    constructor() {
        this.personnes = [];
    }

    ajouterPersonne(personne: Personne): void {
        this.personnes.push(personne);
    }

    trouverPersonneParNom(nom: string): Personne | undefined {
        return this.personnes.find(p => p.nom === nom);
    }

    listeDesPersonnesMariées(): Array<Personne> {
        return this.personnes.filter(p => p.statutMarital === "Marié");
    }

    moyenneAgeDesPersonnes(): number {
        let sommeAge = 0;
        this.personnes.forEach(p => sommeAge += p.âge);
        return sommeAge / this.personnes.length;
    }
}

const gestionnairePersonnes = new GestionnairePersonnes();

const personne1: Personne = {
    nom: "Dupont",
    prénom: "Jean",
    âge: 35,
    adresses: [{
        rue: "Rue des Lilas",
        codePostal: 75000,
        ville: "Paris"
    },
    {
        rue: "Avenue de la République",
        codePostal: 92100,
        ville: "Boulogne-Billancourt"
    }]
};

const personne2: Personne = {
    nom: "Martin",
    prénom: "Sophie",
    âge: 28,
    adresses: [{
        rue: "Rue du Commerce",
        codePostal: 93100,
        ville: "Montreuil"
    }]
};

gestionnairePersonnes.ajouterPersonne(personne1);
gestionnairePersonnes.ajouterPersonne(personne2);

console.log("Liste des personnes :");
gestionnairePersonnes.personnes.forEach(p => console.log(p.nom + " " + p.prénom));

console.log("Personne trouvée par son nom :");
const personneTrouvee = gestionnairePersonnes.trouverPersonneParNom("Martin");
if (personneTrouvée) {
    console.log(personneTrouvée.nom + " " + personneTrouvée.prénom);
} else {
    console.log("Aucune personne trouvée.");
}

console.log("Liste des personnes mariées :");
gestionnairePersonnes.listeDesPersonnesMariées().forEach(p => console.log(p.nom + " " + p.prénom));

console.log("Moyenne d'âge des personnes :");
console.log(gestionnairePersonnes.moyenneAgeDesPersonnes());
```

**Explication du code :**

Ce code TypeScript implémente une gestion de personnes avec des fonctionnalités telles que :

* Ajout de personnes
* Recherche de personnes par nom
* Filtrage des personnes mariées
* Calcul de la moyenne d'âge des personnes

**Interfaces :**

Les interfaces `Personne` et `Adresse` définissent les types attendus pour les objets représentant les personnes et leurs adresses.

**Type :**

Le type `StatutMarital` est un type d'énumération qui définit les statuts matrimoniaux possibles.

**Classe :**

La classe `GestionnairePersonnes` est responsable de la gestion de la collection de personnes. Elle fournit des méthodes pour ajouter, rechercher, filtrer et calculer des statistiques sur les personnes.

**Exemples d'objets :**

Les objets `personne1` et `personne2` sont des instances de l'interface `Personne`.

**Déclaration et utilisation du gestionnaire :**

Un objet `GestionnairePersonnes` est créé et utilisé pour gérer la collection de personnes.

**Ajout de personnes :**

Les personnes sont ajoutées au gestionnaire à l'aide de la méthode `ajouterPersonne`.

**Recherche de personnes par nom :**

La méthode `trouverPersonneParNom` est utilisée pour rechercher une personne par son nom.

**Filtrage des personnes mariées :**

La méthode `listeDesPersonnesMariées` est utilisée pour filtrer la liste des personnes et renvoyer uniquement celles qui sont mariées.

**Calcul de la moyenne d'âge :**

La méthode `moyenneAgeDesPersonnes` calcule la moyenne d'âge de toutes les personnes dans la collection.

**Console :**

Les sorties de la console affichent les informations sur les personnes et les résultats des opérations de filtrage et de calcul.