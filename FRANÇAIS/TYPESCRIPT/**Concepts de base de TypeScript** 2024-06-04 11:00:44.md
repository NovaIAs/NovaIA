**Classe de Base**

```typescript
export class Entité {
  id: number;
  nom: string;

  constructor(id: number, nom: string) {
    this.id = id;
    this.nom = nom;
  }
}
```

**Classe Fille**

```typescript
export class Client extends Entité {
  dateInscription: Date;

  constructor(id: number, nom: string, dateInscription: Date) {
    super(id, nom);
    this.dateInscription = dateInscription;
  }
}
```

**Interface**

```typescript
export interface IProduit {
  id: number;
  nom: string;
  prix: number;
  stock: number;
}
```

**Enumération**

```typescript
export enum StatutCommande {
  EnCours,
  Validée,
  Annulée
}
```

**Générique**

```typescript
export class Paginateur<T> {
  pageCourante: number;
  taillePage: number;
  totalElements: number;
  éléments: T[];

  constructor(pageCourante: number, taillePage: number, totalElements: number, éléments: T[]) {
    this.pageCourante = pageCourante;
    this.taillePage = taillePage;
    this.totalElements = totalElements;
    this.éléments = éléments;
  }
}
```

**Fonction d'Ordre Supérieur**

```typescript
const trierParNom = (a: Entité, b: Entité) => {
  return a.nom.localeCompare(b.nom);
};
```

**Expression Lambda**

```typescript
const clientsTriés = clients.sort((a, b) => a.dateInscription - b.dateInscription);
```

**Promesse**

```typescript
const getProduits = async (): Promise<IProduit[]> => {
  return new Promise((resolve, reject) => {
    setTimeout(() => {
      resolve([{ id: 1, nom: "Produit 1", prix: 10.0, stock: 5 }]);
    }, 1000);
  });
};
```

**Réduction**

```typescript
const prixTotal = produits.reduce((acc, produit) => acc + produit.prix, 0);
```

**Type d'Alliance**

```typescript
type Personne = {
  nom: string;
  age: number;
};

type PersonneAvecFonction = Personne & {
  direBonjour(): void;
};
```

**Modèle de Conception de Modèle**

```typescript
interface IProduitRepository {
  findAll(): Promise<IProduit[]>;
  findById(id: number): Promise<IProduit>;
  save(produit: IProduit): Promise<void>;
  delete(id: number): Promise<void>;
}

class ProduitRepository implements IProduitRepository {
  async findAll(): Promise<IProduit[]> {
    // Implémentation de la méthode
  }

  async findById(id: number): Promise<IProduit> {
    // Implémentation de la méthode
  }

  async save(produit: IProduit): Promise<void> {
    // Implémentation de la méthode
  }

  async delete(id: number): Promise<void> {
    // Implémentation de la méthode
  }
}
```

**Explications du Code**

* **Classe de Base et Classe Fille:** La classe `Entité` définit les propriétés communes à toutes les entités du domaine, tandis que la classe `Client` hérite de ces propriétés et ajoute des propriétés spécifiques aux clients.
* **Interface:** L'interface `IProduit` définit le contrat pour la classe `Produit`.
* **Enumération:** L'énumération `StatutCommande` représente les différents états d'une commande.
* **Générique:** Le générique `Paginateur` permet de paginer une collection d'éléments de n'importe quel type.
* **Fonction d'Ordre Supérieur:** La fonction `trierParNom` est une fonction qui prend deux entités en argument et renvoie un nombre indiquant comment les trier.
* **Expression Lambda:** L'expression lambda `(a, b) => a.dateInscription - b.dateInscription` est une fonction anonyme qui est utilisée pour trier les clients par date d'inscription.
* **Promesse:** La fonction `getProduits` renvoie une promesse qui sera résolue avec un tableau de produits après un délai de 1 seconde.
* **Réduction:** La fonction `reduce` est utilisée pour calculer le prix total des produits.
* **Type d'Alliance:** Le type d'alliance `PersonneAvecFonction` ajoute une méthode `direBonjour` au type `Personne`.
* **Modèle de Conception de Modèle:** Le modèle de conception de modèle définit une interface pour les classes de référentiel, qui permet de séparer la logique d'accès aux données de la logique métier.