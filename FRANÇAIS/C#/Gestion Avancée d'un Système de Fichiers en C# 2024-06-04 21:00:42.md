**Gestion d'un système de fichiers avancé en C#**

**Classe SystèmeFichiers**

```csharp
public class SystèmeFichiers
{
    private List<Fichier> _fichiers;
    private List<Dossier> _dossiers;

    public SystèmeFichiers()
    {
        _fichiers = new List<Fichier>();
        _dossiers = new List<Dossier>();
    }

    public void AjouterFichier(Fichier fichier)
    {
        _fichiers.Add(fichier);
    }

    public void AjouterDossier(Dossier dossier)
    {
        _dossiers.Add(dossier);
    }

    public IEnumerable<Fichier> RechercherFichiers(string nomFichier)
    {
        return _fichiers.Where(f => f.Nom == nomFichier);
    }

    public IEnumerable<Dossier> RechercherDossiers(string nomDossier)
    {
        return _dossiers.Where(d => d.Nom == nomDossier);
    }

    public void DéplacerFichier(Fichier fichier, Dossier destination)
    {
        fichier.Dossier = destination;
    }

    public void SupprimerFichier(Fichier fichier)
    {
        _fichiers.Remove(fichier);
    }

    public void SupprimerDossier(Dossier dossier)
    {
        // Vérifier si le dossier est vide avant de le supprimer
        if (dossier.Fichiers.Count == 0 && dossier.SousDossiers.Count == 0)
        {
            _dossiers.Remove(dossier);
        }
    }
}
```

**Classe Fichier**

```csharp
public class Fichier
{
    public string Nom { get; set; }
    public string Contenu { get; set; }
    public Dossier Dossier { get; set; }
}
```

**Classe Dossier**

```csharp
public class Dossier
{
    public string Nom { get; set; }
    public List<Fichier> Fichiers { get; set; }
    public List<Dossier> SousDossiers { get; set; }

    public Dossier()
    {
        Fichiers = new List<Fichier>();
        SousDossiers = new List<Dossier>();
    }
}
```

**Exemple d'utilisation**

```csharp
// Créer un système de fichiers
SystèmeFichiers systèmeFichiers = new SystèmeFichiers();

// Ajouter des fichiers et des dossiers au système
Fichier fichier1 = new Fichier { Nom = "fichier1.txt", Contenu = "Contenu du fichier 1" };
Dossier dossier1 = new Dossier { Nom = "dossier1" };
systèmeFichiers.AjouterFichier(fichier1);
systèmeFichiers.AjouterDossier(dossier1);

// Déplacer le fichier dans le dossier
systèmeFichiers.DéplacerFichier(fichier1, dossier1);

// Rechercher des fichiers et des dossiers
IEnumerable<Fichier> fichiersRecherchés = systèmeFichiers.RechercherFichiers("fichier1.txt");
IEnumerable<Dossier> dossiersRecherchés = systèmeFichiers.RechercherDossiers("dossier1");

// Afficher les résultats de la recherche
foreach (Fichier fichier in fichiersRecherchés)
{
    Console.WriteLine($"Fichier trouvé : {fichier.Nom}");
}

foreach (Dossier dossier in dossiersRecherchés)
{
    Console.WriteLine($"Dossier trouvé : {dossier.Nom}");
}

// Supprimer le fichier et le dossier
systèmeFichiers.SupprimerFichier(fichier1);
systèmeFichiers.SupprimerDossier(dossier1);
```