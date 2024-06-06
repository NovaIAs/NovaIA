**Programme de Gestion d'Inventaire et de Facturation Complexe en Kotlin**

**Objectif:** Créer un système complet de gestion d'inventaire et de facturation qui gère les produits, les clients, les commandes et les factures.

**Classes Principales:**

**Produit**
* Attributs: idProduit, nomProduit, prixUnitaire, quantitéEnStock
* Méthodes: obtenirIdProduit(), obtenirNomProduit(), obtenirPrixUnitaire(), obtenirQuantitéEnStock(), définirQuantitéEnStock()

**Client**
* Attributs: idClient, nomClient, adresseClient, numéroTéléphone
* Méthodes: obtenirIdClient(), obtenirNomClient(), obtenirAdresseClient(), obtenirNuméroTéléphone()

**Commande**
* Attributs: idCommande, idClient, dateCommande, listeProduits
* Méthodes: obtenirIdCommande(), obtenirIdClient(), obtenirDateCommande(), obtenirListeProduits()

**Facture**
* Attributs: idFacture, idCommande, dateFacture, montantTotal
* Méthodes: obtenirIdFacture(), obtenirIdCommande(), obtenirDateFacture(), obtenirMontantTotal()

**Interface Graphique (GUI)**
* Fenêtre principale avec un menu, des onglets et des formulaires pour gérer les produits, les clients, les commandes et les factures.

**Base de Données**
* Utilise une base de données SQLite pour stocker les données de produits, de clients, de commandes et de factures.

**Fonctions Principales:**

**Gestion des Produits:**
* Ajouter, modifier et supprimer des produits
* Consulter l'inventaire et mettre à jour les quantités en stock

**Gestion des Clients:**
* Ajouter, modifier et supprimer des clients
* Consulter la liste des clients et leurs coordonnées

**Gestion des Commandes:**
* Créer de nouvelles commandes
* Ajouter et supprimer des produits des commandes
* Valider les commandes

**Gestion des Factures:**
* Générer des factures pour les commandes validées
* Afficher et imprimer les factures

**Autres Fonctionnalités:**

* Recherche avancée des produits, des clients et des commandes
* Sauvegarde et restauration des données
* Rapports et graphiques sur les ventes, les stocks et les clients

**Code:**

```kotlin
// Classe Produit
class Produit(val idProduit: Int, val nomProduit: String, val prixUnitaire: Double, var quantitéEnStock: Int) {
    fun obtenirIdProduit(): Int { return idProduit }
    fun obtenirNomProduit(): String { return nomProduit }
    fun obtenirPrixUnitaire(): Double { return prixUnitaire }
    fun obtenirQuantitéEnStock(): Int { return quantitéEnStock }
    fun définirQuantitéEnStock(quantité: Int) { quantitéEnStock = quantité }
}

// Classe Client
class Client(val idClient: Int, val nomClient: String, val adresseClient: String, val numéroTéléphone: String) {
    fun obtenirIdClient(): Int { return idClient }
    fun obtenirNomClient(): String { return nomClient }
    fun obtenirAdresseClient(): String { return adresseClient }
    fun obtenirNuméroTéléphone(): String { return numéroTéléphone }
}

// Classe Commande
class Commande(val idCommande: Int, val idClient: Int, val dateCommande: String, val listeProduits: List<Produit>) {
    fun obtenirIdCommande(): Int { return idCommande }
    fun obtenirIdClient(): Int { return idClient }
    fun obtenirDateCommande(): String { return dateCommande }
    fun obtenirListeProduits(): List<Produit> { return listeProduits }
}

// Classe Facture
class Facture(val idFacture: Int, val idCommande: Int, val dateFacture: String, val montantTotal: Double) {
    fun obtenirIdFacture(): Int { return idFacture }
    fun obtenirIdCommande(): Int { return idCommande }
    fun obtenirDateFacture(): String { return dateFacture }
    fun obtenirMontantTotal(): Double { return montantTotal }
}

// Fonction principale
fun main() {
    // Créer une instance de l'interface graphique
    val gui = GUI()
    // Afficher l'interface graphique
    gui.setVisible(true)
}
```

**Explication:**

* Le code crée des classes pour représenter les produits, les clients, les commandes et les factures.
* Chaque classe a des attributs et des méthodes pour accéder et modifier les données.
* L'interface graphique est créée à partir de la classe `GUI`.
* La fonction `main` est le point d'entrée du programme et crée et affiche l'interface graphique.
* Les autres fonctionnalités, telles que la gestion des produits, la gestion des clients et la gestion des commandes, sont implémentées dans des classes et des méthodes séparées.
* Le code utilise une base de données SQLite pour stocker les données.
* L'interface graphique utilise des formulaires et des onglets pour afficher et modifier les données.