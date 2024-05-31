**Classe abstraite pour définir les composants de l'application**

```typescript
export abstract class Composant {
  protected nom: string;

  constructor(nom: string) {
    this.nom = nom;
  }

  abstract afficher(): void;

  abstract masquer(): void;
}
```

**Classe concrète pour les composants de type texte**

```typescript
export class Texte extends Composant {
  private contenu: string;

  constructor(nom: string, contenu: string) {
    super(nom);
    this.contenu = contenu;
  }

  afficher(): void {
    console.log(`Affichage du texte ${this.nom} : ${this.contenu}`);
  }

  masquer(): void {
    console.log(`Masquage du texte ${this.nom}`);
  }
}
```

**Classe concrète pour les composants de type image**

```typescript
export class Image extends Composant {
  private chemin: string;

  constructor(nom: string, chemin: string) {
    super(nom);
    this.chemin = chemin;
  }

  afficher(): void {
    console.log(`Affichage de l'image ${this.nom} : ${this.chemin}`);
  }

  masquer(): void {
    console.log(`Masquage de l'image ${this.nom}`);
  }
}
```

**Classe concrète pour les composants de type groupe**

```typescript
export class Groupe extends Composant {
  private composants: Composant[];

  constructor(nom: string, ...composants: Composant[]) {
    super(nom);
    this.composants = composants;
  }

  ajouterComposant(composant: Composant): void {
    this.composants.push(composant);
  }

  afficher(): void {
    console.log(`Affichage du groupe ${this.nom} :`);
    this.composants.forEach(composant => composant.afficher());
  }

  masquer(): void {
    console.log(`Masquage du groupe ${this.nom}`);
    this.composants.forEach(composant => composant.masquer());
  }
}
```

**Classe principale pour assembler les composants et gérer l'interface utilisateur**

```typescript
export class Application {
  private composants: Composant[];

  constructor() {
    this.composants = [];
  }

  créerComposantTexte(nom: string, contenu: string): void {
    this.composants.push(new Texte(nom, contenu));
  }

  créerComposantImage(nom: string, chemin: string): void {
    this.composants.push(new Image(nom, chemin));
  }

  créerComposantGroupe(nom: string, ...composants: Composant[]): void {
    this.composants.push(new Groupe(nom, ...composants));
  }

  afficherTout(): void {
    this.composants.forEach(composant => composant.afficher());
  }

  masquerTout(): void {
    this.composants.forEach(composant => composant.masquer());
  }
}
```

**Utilisation de l'application**

```typescript
const app = new Application();

app.créerComposantTexte("titre", "Titre de l'application");
app.créerComposantImage("logo", "chemin/vers/logo.png");
app.créerComposantGroupe("menu", new Texte("item1", "Item 1"), new Texte("item2", "Item 2"));

app.afficherTout();
// Affichage du texte titre : Titre de l'application
// Affichage de l'image logo : chemin/vers/logo.png
// Affichage du groupe menu :
//  - Affichage du texte item1 : Item 1
//  - Affichage du texte item2 : Item 2

app.masquerTout();
// Masquage du texte titre
// Masquage de l'image logo
// Masquage du groupe menu
//  - Masquage du texte item1
//  - Masquage du texte item2
```

**Explication du code**

- Le code utilise le modèle de conception composite pour créer une hiérarchie de composants qui peuvent être affichés ou masqués individuellement ou collectivement.

- La classe abstraite `Composant` définit l'interface commune pour tous les composants, tandis que les classes concrètes `Texte`, `Image` et `Groupe` implémentent les comportements spécifiques à chaque type de composant.

- La classe `Application` joue le rôle de "composite racine", permettant d'assembler les composants et de gérer l'interface utilisateur.

- Le code utilise également le principe du "pattern builder" pour créer facilement des composants et les ajouter à l'application.