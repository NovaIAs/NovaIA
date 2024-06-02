```javascript
// Création d'une classe abstraite 'Composant'
class Composant {
  constructor(nom) {
    this.nom = nom;
    this.enfants = [];
  }

  ajouterEnfant(enfant) {
    this.enfants.push(enfant);
  }

  afficher() {
    console.log(`Nom du composant : ${this.nom}`);
    for (const enfant of this.enfants) {
      enfant.afficher();
    }
  }
}

// Création d'une classe 'Feuille' qui hérite de 'Composant'
class Feuille extends Composant {
  constructor(nom) {
    super(nom);
  }

  // Surcharge de la méthode 'afficher'
  afficher() {
    console.log(`Nom de la feuille : ${this.nom}`);
  }
}

// Création d'une classe 'Noeud' qui hérite de 'Composant'
class Noeud extends Composant {
  constructor(nom) {
    super(nom);
  }

  // Surcharge de la méthode 'afficher'
  afficher() {
    console.log(`Nom du nœud : ${this.nom}`);
    for (const enfant of this.enfants) {
      enfant.afficher();
    }
  }
}

// Création d'un arbre de composants
const arbre = new Noeud("Arbre");
arbre.ajouterEnfant(new Feuille("Feuille 1"));
arbre.ajouterEnfant(new Noeud("Sous-arbre 1"));
arbre.enfants[1].ajouterEnfant(new Feuille("Feuille 2"));
arbre.enfants[1].ajouterEnfant(new Noeud("Sous-arbre 2"));
arbre.enfants[1].enfants[1].ajouterEnfant(new Feuille("Feuille 3"));

// Affichage de l'arbre
console.log("Structure de l'arbre :");
arbre.afficher();
```

**Explication du code :**

Ce code crée une hiérarchie de composants à l'aide de classes et d'héritage.

* La classe abstraite `Composant` définit une structure commune pour les composants de l'arbre, avec un nom et une liste d'enfants.
* La classe `Feuille` représente les feuilles de l'arbre, qui n'ont pas d'enfants.
* La classe `Noeud` représente les nœuds de l'arbre, qui peuvent avoir des enfants.
* L'arbre est créé en tant qu'instance de la classe `Noeud`, avec des feuilles et des sous-arbres ajoutés en tant qu'enfants.
* La méthode `afficher()` est redéfinie dans les classes `Feuille` et `Noeud` pour afficher les noms des feuilles et des nœuds, ainsi que la structure de l'arbre.