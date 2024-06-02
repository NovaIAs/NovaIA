**Gestionnaire de Bibliothèque en C#**

**Classe "Livre"**

```csharp
public class Livre
{
    public int Id { get; set; }
    public string Titre { get; set; }
    public string Auteur { get; set; }
    public int NbPages { get; set; }
    public decimal Prix { get; set; }
    public int QuantiteStock { get; set; }
    public string Genre { get; set; }
}
```

**Classe "Bibliotheque"**

```csharp
public class Bibliotheque
{
    private List<Livre> _livres;

    public Bibliotheque()
    {
        _livres = new List<Livre>();
    }

    public void AjouterLivre(Livre livre)
    {
        _livres.Add(livre);
    }

    public void SupprimerLivre(int id)
    {
        var livre = _livres.Find(l => l.Id == id);
        if (livre != null)
        {
            _livres.Remove(livre);
        }
    }

    public Livre ObtenirLivre(int id)
    {
        return _livres.Find(l => l.Id == id);
    }

    public List<Livre> RechercherLivres(string critere)
    {
        return _livres.Where(l => l.Titre.Contains(critere) || l.Auteur.Contains(critere) || l.Genre.Contains(critere)).ToList();
    }

    public bool EmprunterLivre(int id, int quantite)
    {
        var livre = _livres.Find(l => l.Id == id);
        if (livre != null && livre.QuantiteStock >= quantite)
        {
            livre.QuantiteStock -= quantite;
            return true;
        }
        return false;
    }

    public bool RendreLivre(int id, int quantite)
    {
        var livre = _livres.Find(l => l.Id == id);
        if (livre != null)
        {
            livre.QuantiteStock += quantite;
            return true;
        }
        return false;
    }
}
```

**Classe "Menu"**

```csharp
public class Menu
{
    private Bibliotheque _bibliotheque;

    public Menu(Bibliotheque bibliotheque)
    {
        _bibliotheque = bibliotheque;
    }

    public void AfficherMenu()
    {
        Console.WriteLine("**MENU DE LA BIBLIOTHÈQUE**");
        Console.WriteLine("--------------------------");
        Console.WriteLine("1. Ajouter un livre");
        Console.WriteLine("2. Supprimer un livre");
        Console.WriteLine("3. Obtenir un livre");
        Console.WriteLine("4. Rechercher des livres");
        Console.WriteLine("5. Emprunter un livre");
        Console.WriteLine("6. Rendre un livre");
        Console.WriteLine("7. Quitter");
    }

    public void ExecuterOptionMenu()
    {
        int option = 0;
        do
        {
            AfficherMenu();
            Console.Write("Entrez une option : ");
            option = int.Parse(Console.ReadLine());

            switch (option)
            {
                case 1:
                    AjouterLivre();
                    break;
                case 2:
                    SupprimerLivre();
                    break;
                case 3:
                    ObtenirLivre();
                    break;
                case 4:
                    RechercherLivres();
                    break;
                case 5:
                    EmprunterLivre();
                    break;
                case 6:
                    RendreLivre();
                    break;
                case 7:
                    Console.WriteLine("Au revoir !");
                    break;
                default:
                    Console.WriteLine("Option invalide. Veuillez réessayer.");
                    break;
            }
        } while (option != 7);
    }

    private void AjouterLivre()
    {
        Console.Write("Entrez le titre du livre : ");
        string titre = Console.ReadLine();
        Console.Write("Entrez l'auteur du livre : ");
        string auteur = Console.ReadLine();
        Console.Write("Entrez le nombre de pages du livre : ");
        int nbPages = int.Parse(Console.ReadLine());
        Console.Write("Entrez le prix du livre : ");
        decimal prix = decimal.Parse(Console.ReadLine());
        Console.Write("Entrez la quantité en stock du livre : ");
        int quantiteStock = int.Parse(Console.ReadLine());
        Console.Write("Entrez le genre du livre : ");
        string genre = Console.ReadLine();

        var livre = new Livre
        {
            Titre = titre,
            Auteur = auteur,
            NbPages = nbPages,
            Prix = prix,
            QuantiteStock = quantiteStock,
            Genre = genre
        };

        _bibliotheque.AjouterLivre(livre);
        Console.WriteLine("Livre ajouté avec succès.");
    }

    private void SupprimerLivre()
    {
        Console.Write("Entrez l'identifiant du livre à supprimer : ");
        int id = int.Parse(Console.ReadLine());

        _bibliotheque.SupprimerLivre(id);
        Console.WriteLine("Livre supprimé avec succès.");
    }

    private void ObtenirLivre()
    {
        Console.Write("Entrez l'identifiant du livre à obtenir : ");
        int id = int.Parse(Console.ReadLine());

        var livre = _bibliotheque.ObtenirLivre(id);
        if (livre != null)
        {
            Console.WriteLine($"Titre : {livre.Titre}");
            Console.WriteLine($"Auteur : {livre.Auteur}");
            Console.WriteLine($"Nombre de pages : {livre.NbPages}");
            Console.WriteLine($"Prix : {livre.Prix}");
            Console.WriteLine($"Quantité en stock : {livre.QuantiteStock}");
            Console.WriteLine($"Genre : {livre.Genre}");
        }
        else
        {
            Console.WriteLine("Aucun livre trouvé avec cet identifiant.");
        }
    }

    private void RechercherLivres()
    {
        Console.Write("Entrez le critère de recherche : ");
        string critere = Console.ReadLine();

        var livres = _bibliotheque.RechercherLivres(critere);
        if (livres.Count > 0)
        {
            Console.WriteLine("Livres trouvés :");
            foreach (var livre in livres)
            {
                Console.WriteLine($"Titre : {livre.Titre}");
                Console.WriteLine($"Auteur : {livre.Auteur}");
            }
        }
        else
        {
            Console.WriteLine("Aucun livre trouvé avec ce critère.");
        }
    }

    private void EmprunterLivre()
    {
        Console.Write("Entrez l'identifiant du livre à emprunter : ");
        int id = int.Parse(Console.ReadLine());
        Console.Write("Entrez la quantité à emprunter : ");
        int quantite = int.Parse(Console.ReadLine());

        if (_bibliotheque.EmprunterLivre(id, quantite))
        {
            Console.WriteLine("Livre emprunté avec succès.");
        }
        else
        {
            Console.WriteLine("Erreur lors de l'emprunt du livre. Veuillez vérifier la quantité en stock.");
        }
    }

    private void RendreLivre()
    {
        Console.Write("Entrez l'identifiant du livre à rendre : ");
        int id = int.Parse(Console.ReadLine());
        Console.Write("Entrez la quantité à rendre : ");
        int quantite = int.Parse(Console.ReadLine());

        if (_bibliotheque.RendreLivre(id, quantite))
        {
            Console.WriteLine("Livre rendu avec succès.");
        }
        else
        {
            Console.WriteLine("Erreur lors du rendu du livre. Veuillez vérifier l'identifiant du livre.");
        }
    }
}
```

**Classe "Program"**

```csharp
public class Program
{
    public static void Main(string[] args)
    {
        var bibliotheque = new Bibliotheque();
        var menu = new Menu(bibliotheque);

        menu.ExecuterOptionMenu();
    }
}
```

**Utilisation**

Pour utiliser cette application, exécutez le programme "Program.cs". Le menu s'affichera à l'écran avec les différentes options. Vous pouvez alors choisir l'option souhaitée et entrer les informations requises.

L'application permet d'effectuer les opérations suivantes sur la bibliothèque :

* Ajouter un livre
* Supprimer un livre
* Obtenir un livre
* Rechercher des livres
* Emprunter un livre
* Rendre un livre