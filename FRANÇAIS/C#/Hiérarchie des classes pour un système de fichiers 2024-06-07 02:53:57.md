**Objectif :** Créer une hiérarchie de classes abstraites et concrètes pour représenter un système de fichiers.

**Code :**

```csharp
// Interface racine représentant un élément générique du système de fichiers
public interface IFileSystemItem
{
    string Nom { get; set; }
    DateTime DateCreation { get; set; }
    DateTime DateModification { get; set; }

    void AjouterElement(IFileSystemItem element);
    void SupprimerElement(IFileSystemItem element);
    int Taille { get; }
}

// Classe abstraite représentant un répertoire
public abstract class Repertoire : IFileSystemItem
{
    private List<IFileSystemItem> _elements;

    public Repertoire(string nom)
    {
        Nom = nom;
        _elements = new List<IFileSystemItem>();
    }

    public string Nom { get; set; }
    public DateTime DateCreation { get; set; }
    public DateTime DateModification { get; set; }

    public void AjouterElement(IFileSystemItem element)
    {
        _elements.Add(element);
    }

    public void SupprimerElement(IFileSystemItem element)
    {
        _elements.Remove(element);
    }

    public int Taille
    {
        get
        {
            int taille = 0;
            foreach (var element in _elements)
            {
                taille += element.Taille;
            }
            return taille;
        }
    }
}

// Classe concrète représentant un répertoire racine
public class RepertoireRacine : Repertoire
{
    public RepertoireRacine() : base("Racine") { }
}

// Classe concrète représentant un sous-répertoire
public class SousRepertoire : Repertoire
{
    public SousRepertoire(string nom, Repertoire parent) : base(nom)
    {
        Parent = parent;
    }

    public Repertoire Parent { get; set; }
}

// Classe abstraite représentant un fichier
public abstract class Fichier : IFileSystemItem
{
    public Fichier(string nom, string contenu)
    {
        Nom = nom;
        Contenu = contenu;
    }

    public string Nom { get; set; }
    public DateTime DateCreation { get; set; }
    public DateTime DateModification { get; set; }
    public string Contenu { get; set; }

    public int Taille
    {
        get { return Contenu.Length; }
    }
}

// Classe concrète représentant un fichier texte
public class FichierTexte : Fichier
{
    public FichierTexte(string nom, string contenu) : base(nom, contenu) { }
}

// Classe concrète représentant un fichier binaire
public class FichierBinaire : Fichier
{
    public FichierBinaire(string nom, byte[] contenu) : base(nom, Convert.ToBase64String(contenu)) { }
}

// Exemple d'utilisation
var systemeFichiers = new RepertoireRacine();

var repertoireDocuments = new SousRepertoire("Documents", systemeFichiers);
systemeFichiers.AjouterElement(repertoireDocuments);

var fichierTexte = new FichierTexte("CV.txt", "Mon curriculum vitae");
repertoireDocuments.AjouterElement(fichierTexte);

var fichierBinaire = new FichierBinaire("Image.jpg", new byte[] { 0x01, 0x02, 0x03 });
repertoireDocuments.AjouterElement(fichierBinaire);

Console.WriteLine("Taille du système de fichiers : {0} octets", systemeFichiers.Taille);
```

**Explication :**

* **Interfaces et classes abstraites :** `IFileSystemItem` définit les propriétés et méthodes communes à tous les éléments du système de fichiers, tandis que `Repertoire` et `Fichier` fournissent des implémentations communes pour les répertoires et les fichiers.
* **Classes concrètes :** `RepertoireRacine`, `SousRepertoire`, `FichierTexte` et `FichierBinaire` implémentent des types spécifiques d'éléments du système de fichiers.
* **Propriétés et méthodes :** Les classes fournissent des propriétés pour accéder au nom, à la date de création, à la date de modification et à la taille des éléments du système de fichiers. Elles incluent également des méthodes pour ajouter et supprimer des éléments.
* **Calcul de la taille :** La taille des répertoires est calculée de manière récursive en additionnant les tailles de leurs éléments. La taille des fichiers est basée sur la longueur de leur contenu.
* **Exemple d'utilisation :** Le code crée un système de fichiers hiérarchique avec un répertoire racine, un sous-répertoire et deux fichiers. Il affiche ensuite la taille totale du système de fichiers.