```typescript
// Classe abstraite définissant un élément de menu
abstract class MenuElement {
    // Nom de l'élément
    private _nom: string;

    // Constructeur
    constructor(nom: string) {
        this._nom = nom;
    }

    // Getter pour le nom
    get nom(): string {
        return this._nom;
    }

    // Méthode abstraite pour calculer le coût
    abstract calculerCout(): number;
}

// Classe concrète représentant un plat
class Plat extends MenuElement {
    // Prix du plat
    private _prix: number;

    // Constructeur
    constructor(nom: string, prix: number) {
        super(nom);
        this._prix = prix;
    }

    // Calcul du coût du plat
    calculerCout(): number {
        return this._prix;
    }
}

// Classe concrète représentant un menu
class Menu extends MenuElement {
    // Liste des éléments du menu
    private _elements: MenuElement[];

    // Constructeur
    constructor(nom: string, elements: MenuElement[]) {
        super(nom);
        this._elements = elements;
    }

    // Calcul du coût du menu
    calculerCout(): number {
        return this._elements.reduce((total, element) => total + element.calculerCout(), 0);
    }
}

// Classe concrète représentant une commande
class Commande {
    // Liste des éléments de la commande
    private _elements: MenuElement[];

    // Constructeur
    constructor(elements: MenuElement[]) {
        this._elements = elements;
    }

    // Calcul du coût total de la commande
    calculerCoutTotal(): number {
        return this._elements.reduce((total, element) => total + element.calculerCout(), 0);
    }

    // Affichage de la commande
    afficher(): void {
        console.log("Commande : ");
        for (let element of this._elements) {
            console.log("- " + element.nom + " (" + element.calculerCout() + " €)");
        }
        console.log("Coût total : " + this.calculerCoutTotal() + " €");
    }
}

// Création d'un menu avec des plats
let menu = new Menu("Menu du jour", [
    new Plat("Salade de tomates", 5),
    new Plat("Steak frites", 10),
    new Plat("Crème brûlée", 6)
]);

// Création d'une commande à partir du menu
let commande = new Commande([
    menu.elements[0],
    menu.elements[1]
]);

// Affichage de la commande
commande.afficher();
```

**Explication du code :**

Ce code définit une hiérarchie de classes pour représenter un menu, des plats et des commandes dans un restaurant.

**Classe MenuElement :**

* Représente un élément de menu abstrait avec un nom.
* Possède une méthode abstraite `calculerCout()` pour calculer le coût de l'élément.

**Classe Plat :**

* Hérite de MenuElement et représente un plat spécifique.
* Possède un prix et une méthode `calculerCout()` qui retourne le prix.

**Classe Menu :**

* Hérite de MenuElement et représente un menu contenant une liste d'éléments.
* La méthode `calculerCout()` calcule le coût total du menu en sommant les coûts de ses éléments.

**Classe Commande :**

* Contient une liste d'éléments de menu.
* La méthode `calculerCoutTotal()` calcule le coût total de la commande.
* La méthode `afficher()` affiche le contenu et le coût total de la commande.

**Exemple d'utilisation :**

Le code crée un menu avec trois plats, puis une commande contenant deux éléments du menu. Enfin, il affiche le contenu et le coût total de la commande.