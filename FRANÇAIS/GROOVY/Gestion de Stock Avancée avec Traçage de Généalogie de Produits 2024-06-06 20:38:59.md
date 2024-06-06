**Gestion de Stock Avancée avec Généalogie des Produits**

```groovy
import groovy.transform.EqualsAndHashCode
import groovy.transform.ToString

// Classe de base pour les produits
@EqualsAndHashCode
@ToString
class Produit {
    String id
    String nom
    double prix
    Date dateCréation
}

// Classe pour les produits finis
@EqualsAndHashCode(callSuper = true)
@ToString(includes = "composants")
class ProduitFini extends Produit {
    List<Composant> composants = []
}

// Classe pour les composants
@EqualsAndHashCode
@ToString
class Composant {
    String id
    String nom
    double quantité
    Produit matièrePremière
}

// Classe pour la généalogie des produits
class GenealogieProduit {
    ProduitFini produitFini
    List<Composant> composants
    List<Composant> composantsDeComposants
}

// Initialisation de l'inventaire
List<Produit> inventaire = []
List<GenealogieProduit> genealogies = []

// Création de produits finis et de leurs composants
ProduitFini produitA = new ProduitFini(id: "A", nom: "Produit A")
produitA.composants.addAll([
    new Composant(id: "C1", nom: "Composant 1", quantité: 2, matièrePremière: new Produit(id: "C1", nom: "Matière Première 1")),
    new Composant(id: "C2", nom: "Composant 2", quantité: 1, matièrePremière: new Produit(id: "C2", nom: "Matière Première 2"))
])
inventaire.add(produitA)

ProduitFini produitB = new ProduitFini(id: "B", nom: "Produit B")
produitB.composants.addAll([
    new Composant(id: "C3", nom: "Composant 3", quantité: 3, matièrePremière: new Produit(id: "C3", nom: "Matière Première 3")),
    new Composant(id: "C4", nom: "Composant 4", quantité: 2, matièrePremière: new Produit(id: "C4", nom: "Matière Première 4")),
    new Composant(id: "C5", nom: "Composant 5", quantité: 1, matièrePremière: new Produit(id: "C5", nom: "Matière Première 5"))
])
inventaire.add(produitB)

// Génération de la généalogie des produits
GenealogieProduit genealogieA = new GenealogieProduit()
genealogieA.produitFini = produitA
genealogieA.composants = produitA.composants
genealogieA.composantsDeComposants.addAll([
    new Composant(id: "MP1", nom: "Matière Première 1", quantité: 4),
    new Composant(id: "MP2", nom: "Matière Première 2", quantité: 1)
])
genealogies.add(genealogieA)

GenealogieProduit genealogieB = new GenealogieProduit()
genealogieB.produitFini = produitB
genealogieB.composants = produitB.composants
genealogieB.composantsDeComposants.addAll([
    new Composant(id: "MP3", nom: "Matière Première 3", quantité: 9),
    new Composant(id: "MP4", nom: "Matière Première 4", quantité: 4),
    new Composant(id: "MP5", nom: "Matière Première 5", quantité: 1)
])
genealogies.add(genealogieB)

// Méthode pour calculer le coût total d'un produit fini
double getCoûtTotal(ProduitFini produitFini) {
    double coûtTotal = 0
    for (Composant composant in produitFini.composants) {
        coûtTotal += composant.quantité * composant.matièrePremière.prix
    }
    coûtTotal
}

// Méthode pour afficher l'inventaire et les généalogies des produits
void afficherInventaire() {
    println("**Inventaire**")
    println("Produits finis :")
    inventaire.each { produit ->
        println("\t${produit.nom} (Coût total : ${getCoûtTotal(produit)})")
    }
    println("Généalogies des produits :")
    genealogies.each { genealogie ->
        println("\t${genealogie.produitFini.nom}")
        println("\tComposants :")
        genealogie.composants.each { composant ->
            println("\t\t${composant.nom} (Quantité : ${composant.quantité})")
        }
        println("\tComposants des composants :")
        genealogie.composantsDeComposants.each { composant ->
            println("\t\t${composant.nom} (Quantité : ${composant.quantité})")
        }
    }
}

// Appel de la méthode pour afficher l'inventaire et les généalogies des produits
afficherInventaire()
```

**Explication du code:**

* Les classes `Produit`, `ProduitFini` et `Composant` représentent les entités de base du système de gestion de stock.
* La classe `GenealogieProduit` permet de tracer la généalogie des produits finis, en spécifiant leurs composants et les composants de ces derniers.
* L'inventaire est initialisé avec des produits finis et leurs composants.
* Les généalogies des produits sont générées en fonction de l'inventaire.
* La méthode `getCoûtTotal()` calcule le coût total d'un produit fini en additionnant les coûts de ses composants.
* La méthode `afficherInventaire()` affiche l'inventaire ainsi que les généalogies des produits.