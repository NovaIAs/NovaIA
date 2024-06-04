**Système de gestion des commandes en ligne complexe**

**Objectif:**

* Gérer l'intégralité du processus de commande en ligne, de la réception de la commande à l'expédition.

**Modèle de données:**

**Client:**
* ID
* Nom
* Adresse
* Informations de paiement

**Commande:**
* ID
* Date
* Statut (en attente, traitée, expédiée, livrée)
* Produits commandés

**Produit:**
* ID
* Nom
* Description
* Prix
* Quantité en stock

**Stock:**
* ID de produit
* Quantité en stock

**Processus de commande:**

**1. Réception de la commande:**
* Le client passe une commande via le site Web ou l'API.
* Le système valide les informations de commande et les produits commandés.

**2. Gestion du stock:**
* Le système vérifie la quantité en stock des produits commandés.
* S'il y a suffisamment de stock, le statut de la commande passe à "Traitée".
* Sinon, le système met la commande en attente jusqu'à ce que les produits soient à nouveau en stock.

**3. Traitement du paiement:**
* Le système traite le paiement du client via une passerelle de paiement intégrée.
* Si le paiement est accepté, le statut de la commande passe à "Expédiée".

**4. Préparation de l'expédition:**
* Le système génère une étiquette d'expédition et prépare les produits pour l'expédition.
* Les produits sont emballés et remis à un transporteur.

**5. Suivi de l'expédition:**
* Le système fournit un numéro de suivi au client pour qu'il puisse suivre l'état de sa commande.

**6. Livraison:**
* Le transporteur livre la commande à l'adresse du client.
* Le statut de la commande passe à "Livrée".

**Code Scala:**

```scala
// Définition des classes
case class Client(id: Int, nom: String, adresse: String, paymentInfo: String)
case class Commande(id: Int, date: String, statut: String, produits: List[Produit])
case class Produit(id: Int, nom: String, description: String, prix: Double, stock: Int)

// Définition du système de gestion des commandes
class GestionnaireCommandes {

  // Réception de la commande
  def recevoirCommande(commande: Commande): Unit = {
    // Validation des informations de commande...
    // Gestion du stock...
  }

  // Traitement du paiement
  def traiterPaiement(commande: Commande): Unit = {
    // Interaction avec la passerelle de paiement...
    // Mise à jour du statut de la commande...
  }

  // Préparation de l'expédition
  def preparerExpedition(commande: Commande): Unit = {
    // Génération de l'étiquette d'expédition...
    // Emballage des produits...
  }

  // Suivi de l'expédition
  def suivreExpedition(commande: Commande): String = {
    // Interaction avec le transporteur...
    // Récupération du numéro de suivi...
  }

  // Livraison
  def livrerCommande(commande: Commande): Unit = {
    // Mise à jour du statut de la commande...
  }
}
```

**Utilisation:**

```scala
// Création d'un système de gestion des commandes
val gestionnaireCommandes = new GestionnaireCommandes()

// Test du système
val client = Client(1, "Jean Dupont", "123 Main Street", "VISA 1234567890")
val commande = Commande(1, "2023-03-08", "En attente", List(Produit(1, "Livre", "Un livre passionnant", 10.0, 10)))

// Réception de la commande
gestionnaireCommandes.recevoirCommande(commande)

// Traitement du paiement
gestionnaireCommandes.traiterPaiement(commande)

// Préparation de l'expédition
gestionnaireCommandes.preparerExpedition(commande)

// Suivi de l'expédition
val numeroSuivi = gestionnaireCommandes.suivreExpedition(commande)

// Livraison
gestionnaireCommandes.livrerCommande(commande)
```