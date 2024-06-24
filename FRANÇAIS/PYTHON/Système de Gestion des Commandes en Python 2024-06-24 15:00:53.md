**Système de gestion des commandes en Python**

**Classe Commande**

```python
class Commande:
    def __init__(self, numero_commande, date_commande, client, produits, montant_total):
        self.numero_commande = numero_commande
        self.date_commande = date_commande
        self.client = client
        self.produits = produits
        self.montant_total = montant_total

    def ajouter_produit(self, produit):
        self.produits.append(produit)

    def calculer_montant_total(self):
        for produit in self.produits:
            self.montant_total += produit.prix * produit.quantite

    def afficher_commande(self):
        print("N° commande : {}".format(self.numero_commande))
        print("Date commande : {}".format(self.date_commande))
        print("Client : {}".format(self.client.nom_client))
        for produit in self.produits:
            print("{} : quantité {} - prix unitaire {}".format(
                produit.nom_produit, produit.quantite, produit.prix
            ))
        print("Montant total : {}".format(self.montant_total))
```

**Classe Produit**

```python
class Produit:
    def __init__(self, nom_produit, prix, quantite):
        self.nom_produit = nom_produit
        self.prix = prix
        self.quantite = quantite
```

**Classe Client**

```python
class Client:
    def __init__(self, nom_client, adresse, email, telephone):
        self.nom_client = nom_client
        self.adresse = adresse
        self.email = email
        self.telephone = telephone
```

**Fonction principale**

```python
def main():
    # Créer un client
    client = Client("Dupont", "1 rue du Commerce", "dupont@email.com", "0123456789")

    # Créer une commande
    commande = Commande("C1234", "2023-03-08", client, [], 0)

    # Ajouter des produits à la commande
    commande.ajouter_produit(Produit("Ordinateur", 1000, 1))
    commande.ajouter_produit(Produit("Souris", 20, 2))
    commande.ajouter_produit(Produit("Clavier", 30, 1))

    # Calculer le montant total de la commande
    commande.calculer_montant_total()

    # Afficher la commande
    commande.afficher_commande()

if __name__ == "__main__":
    main()
```

**Explication du code**

Ce code définit un système de gestion des commandes avec des classes pour les commandes, les produits et les clients. La fonction `main()` crée un client, une commande et ajoute des produits à la commande. Le montant total de la commande est ensuite calculé et la commande est affichée.