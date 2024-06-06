**Gestion complexe des stocks dans un entrepôt**

```typescript
// Imports nécessaires
import { GestionnaireStock, Produit, Fournisseur } from "./modules/gestionnaire-stock";

// Création d'un gestionnaire de stock
const gestionnaireStock = new GestionnaireStock();

// Création de produits
const produit1 = new Produit("Téléphone portable", 100, 200);
const produit2 = new Produit("Ordinateur portable", 50, 300);
const produit3 = new Produit("Imprimante", 20, 400);

// Ajout des produits au stock
gestionnaireStock.ajouterProduit(produit1);
gestionnaireStock.ajouterProduit(produit2);
gestionnaireStock.ajouterProduit(produit3);

// Création de fournisseurs
const fournisseur1 = new Fournisseur("Apple", "123 Main Street");
const fournisseur2 = new Fournisseur("Dell", "456 Oak Avenue");
const fournisseur3 = new Fournisseur("HP", "789 Pine Road");

// Affectation des fournisseurs aux produits
produit1.ajouterFournisseur(fournisseur1);
produit1.ajouterFournisseur(fournisseur2);
produit2.ajouterFournisseur(fournisseur2);
produit2.ajouterFournisseur(fournisseur3);
produit3.ajouterFournisseur(fournisseur3);

// Gestion des commandes client
const commandeClient1 = {
  produits: [
    {
      produitId: produit1.id,
      quantite: 10,
    },
    {
      produitId: produit3.id,
      quantite: 5,
    },
  ],
};

// Traitement de la commande
const result = gestionnaireStock.traiterCommande(commandeClient1);

// Affichage du résultat
if (result.succes) {
  console.log("Commande traitée avec succès.");
} else {
  console.log("Erreur lors du traitement de la commande.");
}
```

**Explication du code**

Ce code met en œuvre un système de gestion de stocks complexe avec les fonctionnalités suivantes :

* Gestion des produits, avec quantité en stock et prix ;
* Gestion des fournisseurs ;
* Affectation de fournisseurs aux produits ;
* Traitement des commandes client, en vérifiant la disponibilité des produits en stock et en mettant à jour les niveaux de stock.

Le code utilise des classes et des interfaces TypeScript pour représenter les différents concepts du système et leurs relations. Les classes `Produit`, `Fournisseur` et `GestionnaireStock` définissent les propriétés et les méthodes des objets respectifs.

Le code est bien structuré et organisé en modules pour la gestion des stocks, des produits et des fournisseurs, facilitant ainsi sa maintenance et son évolutivité.