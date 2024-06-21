**Gestion d'un inventaire de produits avec Groovy**

Ce code Groovy gère un inventaire de produits, permettant d'ajouter, de supprimer, de modifier et de rechercher des produits. Il utilise des structures de données avancées telles que les cartes et les listes, et met en œuvre des algorithmes de recherche efficaces pour assurer des performances optimales.

```groovy
// Classe Produit représentant un seul élément de l'inventaire
class Produit {
    String nom
    double prix
    int quantité
}

// La classe Inventaire contient la collection de produits et les méthodes de gestion
class Inventaire {

    // Carte des produits indexés par leur nom pour un accès rapide
    def produits = [:]

    // Ajoute un produit à l'inventaire
    void ajouterProduit(Produit produit) {
        produits[produit.nom] = produit
    }

    // Supprime un produit de l'inventaire par son nom
    void supprimerProduit(String nom) {
        produits.remove(nom)
    }

    // Modifie un produit existant dans l'inventaire
    void modifierProduit(Produit produit) {
        produits[produit.nom] = produit
    }

    // Recherche un produit dans l'inventaire par son nom
    Produit rechercherProduit(String nom) {
        return produits[nom]
    }

    // Recherche tous les produits dont le nom correspond à la chaîne de recherche
    List<Produit> rechercherProduits(String chaineRecherche) {
        // Liste pour stocker les résultats de la recherche
        def résultats = []

        // Itération sur la collection de produits
        produits.each { nom, produit ->
            // Si le nom du produit contient la chaîne de recherche
            if (nom.contains(chaineRecherche)) {
                // Ajout du produit à la liste des résultats
                résultats.add(produit)
            }
        }

        // Retourne la liste des résultats
        return résultats
    }

    // Trie la collection de produits par ordre croissant de prix
    List<Produit> trierParPrix() {
        // Convertit la carte en une liste pour le tri
        def listeProduits = produits.values() as List

        // Trie la liste en utilisant une fonction de comparaison personnalisée
        listeProduits.sort { a, b -> a.prix <=> b.prix }

        // Retourne la liste triée
        return listeProduits
    }
}

// Instanciation d'un inventaire
def inventaire = new Inventaire()

// Ajout de quelques produits à l'inventaire
inventaire.ajouterProduit(new Produit(nom: "Pomme", prix: 1.25, quantité: 10))
inventaire.ajouterProduit(new Produit(nom: "Orange", prix: 1.50, quantité: 20))
inventaire.ajouterProduit(new Produit(nom: "Banane", prix: 1.75, quantité: 15))

// Recherche d'un produit par son nom
def pomme = inventaire.rechercherProduit("Pomme")

// Modification du prix de la pomme
pomme.prix = 1.35
inventaire.modifierProduit(pomme)

// Recherche de produits dont le nom contient "a"
def résultatsRecherche = inventaire.rechercherProduits("a")

// Affichage des résultats de la recherche
résultatsRecherche.each { println it }

// Tri de la collection de produits par prix
def produitsTriés = inventaire.trierParPrix()

// Affichage des produits triés
produitsTriés.each { println it }
```

**Explication du code**

* La classe `Produit` représente un produit dans l'inventaire avec des propriétés telles que `nom`, `prix` et `quantité`.
* La classe `Inventaire` gère la collection des produits et fournit des méthodes pour ajouter, supprimer, modifier et rechercher des produits.
* La méthode `ajouterProduit` ajoute un produit à l'inventaire et l'indexe par son nom pour un accès rapide à l'aide d'une carte.
* La méthode `supprimerProduit` supprime un produit de l'inventaire en fonction de son nom.
* La méthode `modifierProduit` modifie un produit existant dans l'inventaire.
* La méthode `rechercherProduit` recherche un produit dans l'inventaire en fonction de son nom.
* La méthode `rechercherProduits` recherche tous les produits dont le nom correspond à une chaîne de recherche donnée, en utilisant une itération et une vérification de contenu.
* La méthode `trierParPrix` trie la collection de produits par ordre croissant de prix en utilisant une fonction de comparaison personnalisée et le tri de liste.
* Le code principal instancie un inventaire et ajoute quelques produits, puis démontre les différentes méthodes de manipulation et de recherche de l'inventaire.