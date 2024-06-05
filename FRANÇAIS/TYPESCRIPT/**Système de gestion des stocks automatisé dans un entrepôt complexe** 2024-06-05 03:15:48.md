**Objectif du code :** Automatiser le processus de gestion des stocks dans un entrepôt complexe, en tenant compte de plusieurs critères.

```typescript
// Interface représentant un article en stock
interface Article {
  id: number;
  nom: string;
  quantite: number;
  poids: number;
  volume: number;
}

// Classe représentant l'entrepôt
class Entrepot {
  // Liste des articles en stock
  public articles: Article[];

  // Constructeur de l'entrepôt
  constructor() {
    this.articles = [];
  }

  // Ajouter un article à l'entrepôt
  public ajouterArticle(article: Article): void {
    this.articles.push(article);
  }

  // Supprimer un article de l'entrepôt
  public supprimerArticle(id: number): void {
    this.articles = this.articles.filter((article) => article.id !== id);
  }

  // Mettre à jour la quantité d'un article
  public mettreAJourQuantite(id: number, quantite: number): void {
    const article = this.articles.find((article) => article.id === id);
    if (article) {
      article.quantite = quantite;
    }
  }

  // Obtenir un article par son identifiant
  public obtenirArticle(id: number): Article | undefined {
    return this.articles.find((article) => article.id === id);
  }

  // Obtenir la liste des articles triés par poids
  public trierParPoids(): Article[] {
    return this.articles.sort((a, b) => a.poids - b.poids);
  }

  // Obtenir la liste des articles triés par volume
  public trierParVolume(): Article[] {
    return this.articles.sort((a, b) => a.volume - b.volume);
  }

  // Obtenir la liste des articles en stock inférieur ou égal à un seuil donné
  public obtenirArticlesEnBasDeStock(seuil: number): Article[] {
    return this.articles.filter((article) => article.quantite <= seuil);
  }

  // Obtenir la liste des articles en stock supérieur ou égal à un seuil donné
  public obtenirArticlesEnHautDeStock(seuil: number): Article[] {
    return this.articles.filter((article) => article.quantite >= seuil);
  }
}

// Instanciation de l'entrepôt
const entrepot = new Entrepot();

// Ajout de quelques articles à l'entrepôt
entrepot.ajouterArticle({ id: 1, nom: "Article 1", quantite: 10, poids: 10, volume: 10 });
entrepot.ajouterArticle({ id: 2, nom: "Article 2", quantite: 20, poids: 20, volume: 20 });
entrepot.ajouterArticle({ id: 3, nom: "Article 3", quantite: 30, poids: 30, volume: 30 });

// Affichage de la liste des articles triés par poids
console.log("Articles triés par poids :");
console.log(entrepot.trierParPoids());

// Affichage de la liste des articles triés par volume
console.log("Articles triés par volume :");
console.log(entrepot.trierParVolume());

// Affichage de la liste des articles en stock inférieur à 15
console.log("Articles en bas de stock (quantité inférieure à 15) :");
console.log(entrepot.obtenirArticlesEnBasDeStock(15));

// Affichage de la liste des articles en stock supérieur à 25
console.log("Articles en haut de stock (quantité supérieure à 25) :");
console.log(entrepot.obtenirArticlesEnHautDeStock(25));
```

**Explication du code :**

* L'interface `Article` définit les propriétés d'un article en stock, notamment son identifiant, son nom, sa quantité, son poids et son volume.
* La classe `Entrepot` représente l'entrepôt et gère la liste des articles en stock. Elle fournit des méthodes pour ajouter, supprimer, mettre à jour et récupérer des articles.
* Les méthodes `trierParPoids()` et `trierParVolume()` permettent de trier les articles respectivement par poids et par volume.
* Les méthodes `obtenirArticlesEnBasDeStock()` et `obtenirArticlesEnHautDeStock()` permettent de filtrer les articles en fonction de leur quantité.
* Le code d'instanciation de l'entrepôt ajoute quelques articles à l'entrepôt.
* Les lignes de console affichent les résultats des différentes opérations effectuées sur l'entrepôt.