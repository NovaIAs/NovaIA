```typescript
interface IPersonne {
  nom: string;
  prenom: string;
  age: number;
  adresse: IAdresse;
  enfants?: IPersonne[];
}

interface IAdresse {
  rue: string;
  codePostal: string;
  ville: string;
  pays: string;
}

class Personne implements IPersonne {
  nom: string;
  prenom: string;
  age: number;
  adresse: IAdresse;
  enfants?: IPersonne[];

  constructor(nom: string, prenom: string, age: number, adresse: IAdresse, enfants?: IPersonne[]) {
    this.nom = nom;
    this.prenom = prenom;
    this.age = age;
    this.adresse = adresse;
    this.enfants = enfants;
  }

  getNomComplet(): string {
    return `${this.prenom} ${this.nom}`;
  }

  getAge(): number {
    return this.age;
  }

  getAdresse(): IAdresse {
    return this.adresse;
  }

  getEnfants(): IPersonne[] | undefined {
    return this.enfants;
  }
}

class ArbreFamilial {
  membres: IPersonne[];

  constructor(membres: IPersonne[]) {
    this.membres = membres;
  }

  getMembres(): IPersonne[] {
    return this.membres;
  }

  getDescendants(personne: IPersonne): IPersonne[] {
    let descendants: IPersonne[] = [];

    if (personne.enfants) {
      personne.enfants.forEach(enfant => {
        descendants.push(enfant);
        descendants = descendants.concat(this.getDescendants(enfant));
      });
    }

    return descendants;
  }

  getAncetres(personne: IPersonne): IPersonne[] {
    let ancestres: IPersonne[] = [];

    if (personne.parents) {
      personne.parents.forEach(parent => {
        ancestres.push(parent);
        ancestres = ancestres.concat(this.getAncetres(parent));
      });
    }

    return ancestres;
  }
}

const familleDupont = new ArbreFamilial([
  new Personne("Dupont", "Jean", 50, {
    rue: "Rue du Parc",
    codePostal: "75000",
    ville: "Paris",
    pays: "France"
  }),
  new Personne("Dupont", "Marie", 45, {
    rue: "Rue du Parc",
    codePostal: "75000",
    ville: "Paris",
    pays: "France"
  }),
  new Personne("Dupont", "Marc", 20, {
    rue: "Rue du Parc",
    codePostal: "75000",
    ville: "Paris",
    pays: "France"
  }),
  new Personne("Dupont", "Julie", 18, {
    rue: "Rue du Parc",
    codePostal: "75000",
    ville: "Paris",
    pays: "France"
  })
]);

console.log(familleDupont.getMembres());
// [
//   {
//     nom: "Dupont",
//     prenom: "Jean",
//     age: 50,
//     adresse: {
//       rue: "Rue du Parc",
//       codePostal: "75000",
//       ville: "Paris",
//       pays: "France"
//     }
//   },
//   {
//     nom: "Dupont",
//     prenom: "Marie",
//     age: 45,
//     adresse: {
//       rue: "Rue du Parc",
//       codePostal: "75000",
//       ville: "Paris",
//       pays: "France"
//     }
//   },
//   {
//     nom: "Dupont",
//     prenom: "Marc",
//     age: 20,
//     adresse: {
//       rue: "Rue du Parc",
//       codePostal: "75000",
//       ville: "Paris",
//       pays: "France"
//     }
//   },
//   {
//     nom: "Dupont",
//     prenom: "Julie",
//     age: 18,
//     adresse: {
//       rue: "Rue du Parc",
//       codePostal: "75000",
//       ville: "Paris",
//       pays: "France"
//     }
//   }
// ]

console.log(familleDupont.getDescendants(familleDupont.getMembres()[0]));
// [
//   {
//     nom: "Dupont",
//     prenom: "Marc",
//     age: 20,
//     adresse: {
//       rue: "Rue du Parc",
//       codePostal: "75000",
//       ville: "Paris",
//       pays: "France"
//     }
//   },
//   {
//     nom: "Dupont",
//     prenom: "Julie",
//     age: 18,
//     adresse: {
//       rue: "Rue du Parc",
//       codePostal: "75000",
//       ville: "Paris",
//       pays: "France"
//     }
//   }
// ]

console.log(familleDupont.getAncetres(familleDupont.getMembres()[3]));
// []
```

**Explication du code:**

Ce code est un programme écrit en TypeScript qui modélise un arbre familial. Il définit des interfaces et des classes pour représenter les personnes et les arbres familiaux. Le programme crée ensuite un arbre familial nommé "familleDupont" et affiche les membres de la famille, leurs descendants et leurs ancêtres.

Voici une explication détaillée du code:

1. **Interface IPersonne:** Cette interface définit la structure des objets "Personne". Elle spécifie les propriétés obligatoires suivantes : `nom`, `prenom`, `age`, `adresse` et `enfants`.

2. **Interface IAdresse:** Cette interface définit la structure des objets "Adresse". Elle spécifie les propriétés obligatoires suivantes : `rue`, `codePostal`, `ville` et `pays`.

3. **Classe Personne:** Cette classe implémente l'interface IPersonne. Elle représente une personne dans l'arbre familial. Elle définit un constructeur pour initialiser les propriétés de l'objet et fournit des méthodes pour récupérer le nom complet, l'âge, l'adresse et les enfants de la personne.

4. **Classe ArbreFamilial:** Cette classe représente un arbre familial. Elle définit un tableau de membres (IPersonne[]) et fournit des méthodes pour récupérer les membres de l'arbre familial, ainsi que les descendants et les ancêtres d'une personne donnée.

5. **Création de l'arbre familial:** Le programme crée un arbre familial nommé "familleDupont" en instançant la classe ArbreFamilial avec un tableau de membres. Chaque membre est une instance de la classe Personne.

6. **Affichage des membres de la famille:** Le programme affiche les membres de la familleDupont en utilisant la méthode `getMembres()` de la classe ArbreFamilial.

7. **Affichage des descendants:** Le programme affiche les descendants du premier membre de la familleDupont en utilisant la méthode `getDescendants()` de la classe ArbreFamilial.

8. **Affichage des ancêtres:** Le programme affiche les ancêtres du quatrième membre de la familleDupont en utilisant la méthode `getAncetres()` de la classe ArbreFamilial.

Ce code illustre l'utilisation des interfaces, des classes et des méthodes en TypeScript pour modéliser des structures de données complexes.