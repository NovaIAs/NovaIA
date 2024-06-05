**Programme de gestion d'un inventaire complexe**

**Description :**
Ce programme permet de gérer un inventaire d'articles avec des fonctionnalités avancées, notamment la gestion des catégories, des unités de mesure, des fournisseurs et des transactions d'achat et de vente.

**Code :**

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

namespace GestionInventaire
{
    class Program
    {
        // Classes
        public class Article
        {
            public int Id { get; set; }
            public string Nom { get; set; }
            public int CategorieId { get; set; }
            public Categorie Categorie { get; set; }
            public int UniteMesureId { get; set; }
            public UniteMesure UniteMesure { get; set; }
            public decimal PrixAchat { get; set; }
            public decimal PrixVente { get; set; }
            public int QuantiteStock { get; set; }
        }

        public class Categorie
        {
            public int Id { get; set; }
            public string Nom { get; set; }
        }

        public class UniteMesure
        {
            public int Id { get; set; }
            public string Nom { get; set; }
        }

        public class Fournisseur
        {
            public int Id { get; set; }
            public string Nom { get; set; }
            public string Adresse { get; set; }
            public string Ville { get; set; }
            public string Email { get; set; }
            public string Telephone { get; set; }
        }

        public class TransactionAchat
        {
            public int Id { get; set; }
            public int ArticleId { get; set; }
            public Article Article { get; set; }
            public int FournisseurId { get; set; }
            public Fournisseur Fournisseur { get; set; }
            public DateTime DateAchat { get; set; }
            public int QuantiteAchetee { get; set; }
            public decimal PrixUnitaire { get; set; }
            public decimal MontantTotal { get; set; }
        }

        public class TransactionVente
        {
            public int Id { get; set; }
            public int ArticleId { get; set; }
            public Article Article { get; set; }
            public DateTime DateVente { get; set; }
            public int QuantiteVendue { get; set; }
            public decimal PrixUnitaire { get; set; }
            public decimal MontantTotal { get; set; }
        }

        // Méthodes
        public static void Main(string[] args)
        {
            // Contexte de la base de données
            using (var db = new GestionInventaireContext())
            {
                // Initialisation de quelques données
                var categorie1 = new Categorie { Nom = "Informatique" };
                var categorie2 = new Categorie { Nom = "Papeterie" };
                db.Categories.Add(categorie1);
                db.Categories.Add(categorie2);
                db.SaveChanges();

                var unite1 = new UniteMesure { Nom = "Pièce" };
                var unite2 = new UniteMesure { Nom = "Gramme" };
                db.UnitesMesure.Add(unite1);
                db.UnitesMesure.Add(unite2);
                db.SaveChanges();

                var article1 = new Article
                {
                    Nom = "Ordinateur portable",
                    CategorieId = categorie1.Id,
                    UniteMesureId = unite1.Id,
                    PrixAchat = 1000,
                    PrixVente = 1200,
                    QuantiteStock = 10
                };
                var article2 = new Article
                {
                    Nom = "Cahier",
                    CategorieId = categorie2.Id,
                    UniteMesureId = unite2.Id,
                    PrixAchat = 5,
                    PrixVente = 7,
                    QuantiteStock = 50
                };
                db.Articles.Add(article1);
                db.Articles.Add(article2);
                db.SaveChanges();

                var fournisseur1 = new Fournisseur
                {
                    Nom = "Dell",
                    Adresse = "1681 Route de Tournefeuille, 31170 Tournefeuille",
                    Ville = "Tournefeuille",
                    Email = "dell@example.com",
                    Telephone = "05 61 75 77 77"
                };
                var fournisseur2 = new Fournisseur
                {
                    Nom = "Bureau Vallée",
                    Adresse = "Centre Commercial Auchan, 31770 Colomiers",
                    Ville = "Colomiers",
                    Email = "bureauvallee@example.com",
                    Telephone = "05 34 40 26 00"
                };
                db.Fournisseurs.Add(fournisseur1);
                db.Fournisseurs.Add(fournisseur2);
                db.SaveChanges();

                var transactionAchat1 = new TransactionAchat
                {
                    ArticleId = article1.Id,
                    FournisseurId = fournisseur1.Id,
                    DateAchat = DateTime.Now,
                    QuantiteAchetee = 5,
                    PrixUnitaire = 950,
                    MontantTotal = 4750
                };
                var transactionAchat2 = new TransactionAchat
                {
                    ArticleId = article2.Id,
                    FournisseurId = fournisseur2.Id,
                    DateAchat = DateTime.Now,
                    QuantiteAchetee = 100,
                    PrixUnitaire = 4.5m,
                    MontantTotal = 450
                };
                db.TransactionsAchats.Add(transactionAchat1);
                db.TransactionsAchats.Add(transactionAchat2);
                db.SaveChanges();

                var transactionVente1 = new TransactionVente
                {
                    ArticleId = article1.Id,
                    DateVente = DateTime.Now,
                    QuantiteVendue = 2,
                    PrixUnitaire = 1150,
                    MontantTotal = 2300
                };
                var transactionVente2 = new TransactionVente
                {
                    ArticleId = article2.Id,
                    DateVente = DateTime.Now,
                    QuantiteVendue = 75,
                    PrixUnitaire = 6.5m,
                    MontantTotal = 487.5m
                };
                db.TransactionsVentes.Add(transactionVente1);
                db.TransactionsVentes.Add(transactionVente2);
                db.SaveChanges();

                // Affichage des articles en stock
                Console.WriteLine("Articles en stock :");
                foreach (var article in db.Articles)
                {
                    Console.WriteLine($"{article.Nom} | {article.Categorie.Nom} | {article.UniteMesure.Nom} | {article.QuantiteStock}");
                }

                // Calcul du stock total
                var stockTotal = db.Articles.Sum(a => a.QuantiteStock);
                Console.WriteLine($"Stock total : {stockTotal}");

                // Recherche d'un article
                Console.WriteLine("Rechercher un article par nom :");
                var nomArticle = Console.ReadLine();
                var articleRecherche = db.Articles.FirstOrDefault(a => a.Nom == nomArticle);
                if (articleRecherche != null)
                {
                    Console.WriteLine($"Article trouvé : {articleRecherche.Nom}");
                }
                else
                {
                    Console.WriteLine("Article non trouvé.");
                }
            }
        }
    }
}
```

**Explications du code :**

* **Classes :** Représentent les différents types d'entités (articles, catégories, unités de mesure, fournisseurs, transactions d'achat et de vente).

* **Méthode Main :** Point d'entrée du programme. Initialise la base de données, ajoute quelques données et affiche les articles en stock et le stock total.

* **Fonctions de base de données :** Utilisées pour ajouter, supprimer, mettre à jour et récupérer des données de la base de données.

* **Calcul du stock total :** Calcule le nombre total d'articles en stock en sommant les quantités de stock pour tous les articles de la base de données.

* **Recherche d'un article :** Demande à l'utilisateur un nom d'article et le recherche dans la base de données.