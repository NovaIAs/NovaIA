**Système de gestion de bibliothèque en Kotlin**

```kotlin
class Livre(
    val titre: String,
    val auteur: String,
    val isbn: String,
    var emprunté: Boolean = false,
) {
    override fun toString(): String {
        return "$titre - $auteur ($isbn)"
    }
}

class Bibliothèque {
    private val livres: MutableList<Livre> = mutableListOf()

    fun ajouterLivre(livre: Livre) {
        livres.add(livre)
    }

    fun emprunterLivre(isbn: String): Boolean {
        val livre = livres.find { it.isbn == isbn } ?: return false
        if (!livre.emprunté) {
            livre.emprunté = true
            return true
        }
        return false
    }

    fun rendreLivre(isbn: String): Boolean {
        val livre = livres.find { it.isbn == isbn } ?: return false
        if (livre.emprunté) {
            livre.emprunté = false
            return true
        }
        return false
    }

    fun rechercherLivre(critère: String): List<Livre> {
        return livres.filter {
            it.titre.contains(critère) ||
            it.auteur.contains(critère) ||
            it.isbn.contains(critère)
        }
    }

    fun listerLivres() {
        println("Livres disponibles :")
        livres.filter { !it.emprunté }.forEach { println(it) }
    }

    fun listerLivresEmpruntés() {
        println("Livres empruntés :")
        livres.filter { it.emprunté }.forEach { println(it) }
    }
}

fun main() {
    val bibliothèque = Bibliothèque()

    bibliothèque.ajouterLivre(Livre("Le Petit Prince", "Antoine de Saint-Exupéry", "9782070610292"))
    bibliothèque.ajouterLivre(Livre("Le Seigneur des Anneaux", "J.R.R. Tolkien", "9780395082560"))
    bibliothèque.ajouterLivre(Livre("1984", "George Orwell", "9780451524935"))

    bibliothèque.emprunterLivre("9782070610292")
    bibliothèque.emprunterLivre("9780395082560")

    bibliothèque.listerLivres()
    bibliothèque.listerLivresEmpruntés()

    bibliothèque.rendreLivre("9780395082560")
    bibliothèque.listerLivresEmpruntés()

    val résultats = bibliothèque.rechercherLivre("Petit")
    println("Résultats de la recherche :")
    résultats.forEach { println(it) }
}
```

**Explication du code :**

Ce code met en œuvre un système de gestion de bibliothèque qui permet d'ajouter, d'emprunter, de rendre et de rechercher des livres. Il utilise une classe `Livre` pour représenter chaque livre avec un titre, un auteur, un ISBN et un indicateur d'emprunt.

La classe `Bibliothèque` gère une liste de livres et fournit des méthodes pour manipuler cette liste et effectuer des opérations courantes de bibliothèque, comme l'ajout, l'emprunt, le rendu et la recherche de livres.

La fonction `main()` crée une instance de `Bibliothèque`, ajoute des livres, emprunte et rend des livres, puis affiche les livres disponibles et empruntés. Elle effectue également une recherche sur les livres contenant la chaîne "Petit".