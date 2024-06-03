**Gestionnaire d'inventaire Complexe**

```groovy
// Gestionnaire d'Inventaire

// Classe d'inventaire
class Inventaire {

    // Champs
    List<Produit> produits = []
    Map<Produit, Integer> quantites = [:]

    // Constructeur
    Inventaire(List<Produit> produits, Map<Produit, Integer> quantites) {
        this.produits = produits
        this.quantites = quantites
    }

    // Méthodes
    def ajouterProduit(Produit produit, int quantite) {
        if (produits.contains(produit)) {
            quantites[produit] += quantite
        } else {
            produits << produit
            quantites[produit] = quantite
        }
    }

    def retirerProduit(Produit produit, int quantite) {
        if (produits.contains(produit)) {
            def nouvelleQuantite = quantites[produit] - quantite
            if (nouvelleQuantite > 0) {
                quantites[produit] = nouvelleQuantite
            } else {
                produits.remove(produit)
                quantites.remove(produit)
            }
        }
    }

    def afficherInventaire() {
        println "Produits en stock :"
        for (Produit produit : produits) {
            println "\t${produit.nom} : ${quantites[produit]} unités"
        }
    }
}

// Classe de produit
class Produit {

    // Champs
    String nom
    double prix

    // Constructeur
    Produit(String nom, double prix) {
        this.nom = nom
        this.prix = prix
    }

    @Override
    String toString() {
        return nom
    }
}

// Classe principale
class GestionnaireInventaire {

    // Méthode principale
    static void main(String[] args) {

        // Créer des produits
        def produit1 = new Produit("Ordinateur portable", 1000.0)
        def produit2 = new Produit("Souris", 20.0)
        def produit3 = new Produit("Clavier", 50.0)

        // Créer un inventaire
        def inventaire = new Inventaire([produit1, produit2, produit3],
                                      [(produit1): 5, (produit2): 10, (produit3): 20])

        // Ajouter des produits à l'inventaire
        inventaire.ajouterProduit(produit1, 2)
        inventaire.ajouterProduit(produit2, 5)

        // Retirer des produits de l'inventaire
        inventaire.retirerProduit(produit3, 10)

        // Afficher l'inventaire
        inventaire.afficherInventaire()
    }
}
```

**Explication du code :**

* **Classe Inventaire :** Cette classe représente un inventaire et contient une liste de produits et une map pour stocker les quantités de chaque produit.
* **Classe Produit :** Cette classe représente un produit et contient son nom et son prix.
* **Classe GestionnaireInventaire :** Cette classe est la classe principale qui crée des produits, un inventaire et démontre les opérations d'ajout et de retrait de produits de l'inventaire.
* **Méthodes ajouterProduit et retirerProduit :** Ces méthodes ajoutent ou retirent des produits de l'inventaire en mettant à jour les quantités en conséquence.
* **Méthode afficherInventaire :** Cette méthode affiche la liste des produits et leurs quantités en stock.