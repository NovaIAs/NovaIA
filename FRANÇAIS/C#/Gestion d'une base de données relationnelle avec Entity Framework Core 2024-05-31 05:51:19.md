**Gestion d'une Base de Données Relationnelle avec Entity Framework Core**

```csharp
using Microsoft.EntityFrameworkCore;
using Microsoft.Extensions.DependencyInjection;
using System;
using System.Collections.Generic;
using System.Linq;

namespace GestionDeBaseDeDonneesRelationnelle
{
    // Contexte de la base de données
    public class ContexteBaseDeDonnees : DbContext
    {
        public DbSet<Produit> Produits { get; set; }
        public DbSet<Commande> Commandes { get; set; }
        public DbSet<LigneCommande> LignesCommandes { get; set; }

        protected override void OnConfiguring(DbContextOptionsBuilder optionsBuilder)
        {
            optionsBuilder.UseSqlServer("Serveur=localhost;BaseDeDonnees=MaBaseDeDonnees;Utilisateur=utilisateur;MotDePasse=motdepasse");
        }
    }

    // Modèle du produit
    public class Produit
    {
        public int Id { get; set; }
        public string Nom { get; set; }
        public decimal Prix { get; set; }
    }

    // Modèle de la commande
    public class Commande
    {
        public int Id { get; set; }
        public DateTime DateCommande { get; set; }
        public int ClientId { get; set; }
    }

    // Modèle de la ligne de commande
    public class LigneCommande
    {
        public int Id { get; set; }
        public int CommandeId { get; set; }
        public int ProduitId { get; set; }
        public int Quantite { get; set; }
    }

    // Programme principal
    class Program
    {
        static void Main(string[] args)
        {
            // Configuration des services
            var services = new ServiceCollection();
            services.AddDbContext<ContexteBaseDeDonnees>();

            // Création du provider de services
            var provider = services.BuildServiceProvider();

            // Utilisation du contexte de la base de données
            using (var contexte = provider.GetService<ContexteBaseDeDonnees>())
            {
                // Insertion de données
                var produit1 = new Produit { Nom = "Produit 1", Prix = 10.00M };
                var produit2 = new Produit { Nom = "Produit 2", Prix = 15.00M };
                var commande = new Commande { DateCommande = DateTime.Now, ClientId = 1 };
                var ligneCommande1 = new LigneCommande { CommandeId = commande.Id, ProduitId = produit1.Id, Quantite = 2 };
                var ligneCommande2 = new LigneCommande { CommandeId = commande.Id, ProduitId = produit2.Id, Quantite = 3 };
                contexte.Produits.Add(produit1);
                contexte.Produits.Add(produit2);
                contexte.Commandes.Add(commande);
                contexte.LignesCommandes.Add(ligneCommande1);
                contexte.LignesCommandes.Add(ligneCommande2);
                contexte.SaveChanges();

                // Récupération des données
                Console.WriteLine("Produits :");
                foreach (var produit in contexte.Produits)
                {
                    Console.WriteLine($"\t{produit.Id} - {produit.Nom} - {produit.Prix}");
                }

                Console.WriteLine("Commandes :");
                foreach (var commande in contexte.Commandes)
                {
                    Console.WriteLine($"\t{commande.Id} - {commande.DateCommande} - {commande.ClientId}");
                }

                Console.WriteLine("Lignes de commandes :");
                foreach (var ligneCommande in contexte.LignesCommandes)
                {
                    Console.WriteLine($"\t{ligneCommande.Id} - {ligneCommande.CommandeId} - {ligneCommande.ProduitId} - {ligneCommande.Quantite}");
                }
            }
        }
    }
}
```

**Explication du code :**

Ce code implémente un système simple de gestion d'une base de données relationnelle à l'aide de la bibliothèque Entity Framework Core.

* **Modèle de données :**
    * `Produit` : Classe représentant un produit avec des propriétés `Id`, `Nom` et `Prix`.
    * `Commande` : Classe représentant une commande avec des propriétés `Id`, `DateCommande` et `ClientId`.
    * `LigneCommande` : Classe représentant une ligne de commande avec des propriétés `Id`, `CommandeId`, `ProduitId` et `Quantite`.

* **Contexte de la base de données :**
    * `ContexteBaseDeDonnees` : Classe dérivée de `DbContext` qui définit les tables de la base de données et leurs relations.

* **Configuration des services :**
    * La méthode `Main` utilise le conteneur d'injection de dépendances pour configurer les services, y compris le contexte de la base de données.

* **Utilisation du contexte de la base de données :**
    * La méthode `Main` utilise le contexte de la base de données pour insérer, récupérer et afficher les données de la base de données.