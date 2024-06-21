**Gestionnaire de Stock Avancé**

**Objectif :**
Développer un système de gestion de stock avancé qui permet de gérer des articles, des catégories et des fournisseurs.

**Code :**

```kotlin
// Classe Article
data class Article(
    val id: Int,
    val nom: String,
    val description: String,
    val prixUnitaire: Double,
    val stockActuel: Int
)

// Classe Catégorie
data class Categorie(
    val id: Int,
    val nom: String,
    val description: String
)

// Classe Fournisseur
data class Fournisseur(
    val id: Int,
    val nom: String,
    val adresse: String,
    val telephone: String
)

// Interface de service
interface StockService {
    fun ajouterArticle(article: Article)
    fun modifierArticle(article: Article)
    fun supprimerArticle(id: Int)
    fun ajouterCategorie(categorie: Categorie)
    fun modifierCategorie(categorie: Categorie)
    fun supprimerCategorie(id: Int)
    fun ajouterFournisseur(fournisseur: Fournisseur)
    fun modifierFournisseur(fournisseur: Fournisseur)
    fun supprimerFournisseur(id: Int)
    fun getArticles(): List<Article>
    fun getCategories(): List<Categorie>
    fun getFournisseurs(): List<Fournisseur>
}

// Implémentation du service
class StockServiceImpl : StockService {
    // Liste des articles
    private val articles = mutableListOf<Article>()

    // Liste des catégories
    private val categories = mutableListOf<Categorie>()

    // Liste des fournisseurs
    private val fournisseurs = mutableListOf<Fournisseur>()

    override fun ajouterArticle(article: Article) {
        articles.add(article)
    }

    override fun modifierArticle(article: Article) {
        val index = articles.indexOfFirst { it.id == article.id }
        if (index != -1) {
            articles[index] = article
        }
    }

    override fun supprimerArticle(id: Int) {
        articles.removeIf { it.id == id }
    }

    override fun ajouterCategorie(categorie: Categorie) {
        categories.add(categorie)
    }

    override fun modifierCategorie(categorie: Categorie) {
        val index = categories.indexOfFirst { it.id == categorie.id }
        if (index != -1) {
            categories[index] = categorie
        }
    }

    override fun supprimerCategorie(id: Int) {
        categories.removeIf { it.id == id }
    }

    override fun ajouterFournisseur(fournisseur: Fournisseur) {
        fournisseurs.add(fournisseur)
    }

    override fun modifierFournisseur(fournisseur: Fournisseur) {
        val index = fournisseurs.indexOfFirst { it.id == fournisseur.id }
        if (index != -1) {
            fournisseurs[index] = fournisseur
        }
    }

    override fun supprimerFournisseur(id: Int) {
        fournisseurs.removeIf { it.id == id }
    }

    override fun getArticles(): List<Article> = articles

    override fun getCategories(): List<Categorie> = categories

    override fun getFournisseurs(): List<Fournisseur> = fournisseurs
}

**Explication du code :**

* **Classes de données :**
    * Article, Categorie et Fournisseur définissent les objets de base avec des propriétés et des méthodes de base.

* **Interface de service :**
    * StockService définit les opérations que le système doit supporter.

* **Implémentation du service :**
    * StockServiceImpl implémente les opérations de l'interface de service en utilisant des listes mutables pour stocker les données.

Ce code fournit un système de gestion de stock flexible et évolutif qui peut être utilisé pour gérer des informations complexes sur les articles, les catégories et les fournisseurs.