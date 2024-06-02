**Code GROOVY complexe**

```groovy
// **Partie 1 : Déclaration des classes et des objets**

class Livre {
    String titre
    String auteur
    int nbPages
}

class Bibliotheque {
    ArrayList<Livre> livres = new ArrayList<>()

    void ajouterLivre(Livre livre) {
        livres.add(livre)
    }
}

// **Partie 2 : Écriture d'une fonction avec des paramètres optionnels**

def calculerMoyennePages(Bibliotheque bibliotheque, int pagesMin = 0, int pagesMax = Integer.MAX_VALUE) {
    double moyenne = 0.0
    int totalPages = 0
    int nbLivres = 0

    for (Livre livre in bibliotheque.livres) {
        if (livre.nbPages >= pagesMin && livre.nbPages <= pagesMax) {
            totalPages += livre.nbPages
            nbLivres++
        }
    }

    if (nbLivres == 0) {
        moyenne = -1.0
    } else {
        moyenne = totalPages / nbLivres
    }

    return moyenne
}

// **Partie 3 : Utilisation d'expressions lambda et de fermetures**

Bibliotheque maBibliotheque = new Bibliotheque()
maBibliotheque.ajouterLivre(new Livre(titre: "Les Misérables", auteur: "Victor Hugo", nbPages: 1463))
maBibliotheque.ajouterLivre(new Livre(titre: "Le Petit Prince", auteur: "Antoine de Saint-Exupéry", nbPages: 93))
maBibliotheque.ajouterLivre(new Livre(titre: "1984", auteur: "George Orwell", nbPages: 328))

// Calculer la moyenne des pages des livres de plus de 1000 pages
double moyennePages1000 = calculerMoyennePages(maBibliotheque, 1000)

// Utiliser une fermeture pour filtrer les livres de l'auteur "Antoine de Saint-Exupéry"
def livresAuteur = maBibliotheque.livres.findAll { it.auteur == "Antoine de Saint-Exupéry" }

// Afficher les résultats
println("Moyenne des pages des livres de plus de 1000 pages : ${moyennePages1000}")
println("Livres de l'auteur Antoine de Saint-Exupéry : ${livresAuteur}")
```

**Explication du code**

**Partie 1 : Déclaration des classes et des objets**

* La classe `Livre` représente un livre avec ses attributs : titre, auteur et nombre de pages.
* La classe `Bibliotheque` représente une bibliothèque contenant une liste de livres.

**Partie 2 : Écriture d'une fonction avec des paramètres optionnels**

* La fonction `calculerMoyennePages` calcule la moyenne du nombre de pages des livres d'une bibliothèque en fonction des valeurs minimale et maximale de nombre de pages spécifiées (ou des valeurs par défaut si aucune valeur n'est fournie).

**Partie 3 : Utilisation d'expressions lambda et de fermetures**

* Une expression lambda est utilisée pour filtrer les livres d'un auteur spécifique.
* Une fermeture est utilisée pour appeler la fonction `calculerMoyennePages` avec des paramètres spécifiques.

**Résultats**

* La moyenne du nombre de pages des livres de plus de 1000 pages est calculée et affichée.
* La liste des livres de l'auteur spécifié est filtrée et affichée.