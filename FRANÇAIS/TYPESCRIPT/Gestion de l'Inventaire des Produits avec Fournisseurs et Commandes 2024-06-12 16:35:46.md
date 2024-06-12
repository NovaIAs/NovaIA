**Gestion d'un inventaire de produits avec des fournisseurs et des commandes**

```typescript
// Interface pour les produits
interface Produit {
  id: number;
  nom: string;
  description: string;
  prix: number;
  stock: number;
}

// Interface pour les fournisseurs
interface Fournisseur {
  id: number;
  nom: string;
  email: string;
  telephone: string;
  produits: Produit[];
}

// Interface pour les commandes
interface Commande {
  id: number;
  date: Date;
  produits: Produit[];
  fournisseur: Fournisseur;
  statut: "En cours" | "Expédié" | "Reçu";
}

// Classe pour gérer l'inventaire des produits
class Inventaire {
  private produits: Produit[];
  private fournisseurs: Fournisseur[];
  private commandes: Commande[];

  constructor() {
    this.produits = [];
    this.fournisseurs = [];
    this.commandes = [];
  }

  // Méthode pour ajouter un produit à l'inventaire
  ajouterProduit(produit: Produit): void {
    this.produits.push(produit);
  }

  // Méthode pour supprimer un produit de l'inventaire
  supprimerProduit(id: number): void {
    const index = this.produits.findIndex((p) => p.id === id);
    if (index !== -1) {
      this.produits.splice(index, 1);
    }
  }

  // Méthode pour modifier un produit de l'inventaire
  modifierProduit(produitModifie: Produit): void {
    const index = this.produits.findIndex((p) => p.id === produitModifie.id);
    if (index !== -1) {
      this.produits[index] = produitModifie;
    }
  }

  // Méthode pour ajouter un fournisseur à la liste des fournisseurs
  ajouterFournisseur(fournisseur: Fournisseur): void {
    this.fournisseurs.push(fournisseur);
  }

  // Méthode pour supprimer un fournisseur de la liste des fournisseurs
  supprimerFournisseur(id: number): void {
    const index = this.fournisseurs.findIndex((f) => f.id === id);
    if (index !== -1) {
      this.fournisseurs.splice(index, 1);
    }
  }

  // Méthode pour modifier un fournisseur de la liste des fournisseurs
  modifierFournisseur(fournisseurModifie: Fournisseur): void {
    const index = this.fournisseurs.findIndex((f) => f.id === fournisseurModifie.id);
    if (index !== -1) {
      this.fournisseurs[index] = fournisseurModifie;
    }
  }

  // Méthode pour ajouter une commande à la liste des commandes
  ajouterCommande(commande: Commande): void {
    this.commandes.push(commande);
  }

  // Méthode pour supprimer une commande de la liste des commandes
  supprimerCommande(id: number): void {
    const index = this.commandes.findIndex((c) => c.id === id);
    if (index !== -1) {
      this.commandes.splice(index, 1);
    }
  }

  // Méthode pour modifier une commande de la liste des commandes
  modifierCommande(commandeModifiee: Commande): void {
    const index = this.commandes.findIndex((c) => c.id === commandeModifiee.id);
    if (index !== -1) {
      this.commandes[index] = commandeModifiee;
    }
  }

  // Méthode pour rechercher un produit par son nom
  rechercherProduitParNom(nom: string): Produit[] {
    return this.produits.filter((p) => p.nom.toLowerCase().includes(nom.toLowerCase()));
  }

  // Méthode pour rechercher un fournisseur par son nom
  rechercherFournisseurParNom(nom: string): Fournisseur[] {
    return this.fournisseurs.filter((f) => f.nom.toLowerCase().includes(nom.toLowerCase()));
  }

  // Méthode pour rechercher une commande par son numéro
  rechercherCommandeParNum(num: number): Commande | undefined {
    return this.commandes.find((c) => c.id === num);
  }

  // Méthode pour afficher un rapport sur l'inventaire des produits
  afficherRapportInventaire(): void {
    console.log("Rapport d'inventaire des produits :");
    this.produits.forEach((p) => {
      console.log(`- ${p.nom} (${p.stock} en stock)`);
    });
  }
}

// Création d'une instance de la classe Inventaire
const inventaire = new Inventaire();

// Quelques exemples d'utilisation

// Ajout de produits à l'inventaire
inventaire.ajouterProduit({ id: 1, nom: "Pomme", description: "Fruit rouge et sucré", prix: 1.5, stock: 100 });
inventaire.ajouterProduit({ id: 2, nom: "Banane", description: "Fruit jaune et courbé", prix: 1, stock: 50 });

// Ajout de fournisseurs à la liste des fournisseurs
inventaire.ajouterFournisseur({ id: 1, nom: "Pomme Plus", email: "pommeplus@email.com", telephone: "01 23 45 67 89" });
inventaire.ajouterFournisseur({ id: 2, nom: "Banane Fraîche", email: "bananefraiche@email.com", telephone: "02 34 56 78 90" });

// Ajout d'une commande à la liste des commandes
inventaire.ajouterCommande({
  id: 1,
  date: new Date(),
  produits: [{ id: 1, nom: "Pomme", description: "Fruit rouge et sucré", prix: 1.5, stock: 100 }, { id: 2, nom: "Banane", description: "Fruit jaune et courbé", prix: 1, stock: 50 }],
  fournisseur: { id: 1, nom: "Pomme Plus", email: "pommeplus@email.com", telephone: "01 23 45 67 89" },
  statut: "En cours",
});

// Affichage du rapport d'inventaire
inventaire.afficherRapportInventaire();
```

**Explication du code:**

Ce code implémente une gestion d'inventaire de produits avec des fournisseurs et des commandes. Il utilise des classes et des interfaces pour modéliser les différentes entités de l'inventaire.

Les classes principales sont `Produit`, `Fournisseur` et `Commande`. Elles contiennent des champs pour stocker les informations essentielles sur chaque entité.

La classe `Inventaire` gère la collection de produits, fournisseurs et commandes. Elle fournit des méthodes pour ajouter, supprimer et modifier des entités, ainsi que pour rechercher et afficher des informations.

Le code fournit quelques exemples d'utilisation pour démontrer comment utiliser les différentes fonctionnalités de la gestion d'inventaire.