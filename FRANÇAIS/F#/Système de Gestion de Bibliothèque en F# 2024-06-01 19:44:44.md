**Système de Gestion de Bibliothèque en F#**

```f#
// Déclaration des Enums et des Types

type Genre =
    | Roman
    | ScienceFiction
    | Biographie
    | Histoire
    | Théâtre
    | Autre

type EtatLivre =
    | Disponible
    | Emprunté
    | Perdu

type Livre =
    {
        Titre: string
        Auteur: string
        Genre: Genre
        Etat: EtatLivre
        DateEmprunt: DateTime option
        DateRetour: DateTime option
    }

type Lecteur =
    {
        Nom: string
        Prenom: string
        Email: string
        Téléphone: string
        Adresse: string
        LivresEmpruntés: List<Livre>
    }

// Déclaration des Fonctions

// Ajout d'un livre à la bibliothèque
let ajouterLivre(livres: Livre list, livre: Livre) =
    [livre] @ livres

// Recherche d'un livre par son titre
let rechercherLivreParTitre(livres: Livre list, titre: string) =
    livres |> List.find (fun l -> l.Titre = titre)

// Emprunt d'un livre par un lecteur
let emprunterLivre(livres: Livre list, lecteur: Lecteur, livre: Livre) =
    match livre.Etat with
    | Disponible ->
        // Mettre à jour l'état du livre à "Emprunté"
        let livreEmprunté = { livre with Etat = Emprunté; DateEmprunt = DateTime.Now }
        // Ajouter le livre à la liste des livres empruntés par le lecteur
        let lecteurAvecLivre = { lecteur with LivresEmpruntés = livreEmprunté :: lecteur.LivresEmpruntés }
        // Remplacer le livre dans la liste des livres de la bibliothèque
        [livreEmprunté] |> List.replace livres livre
        // Retourner le lecteur avec le livre emprunté
        lecteurAvecLivre
    | _ -> failwith "Le livre n'est pas disponible"

// Retour d'un livre par un lecteur
let retournerLivre(livres: Livre list, lecteur: Lecteur, livre: Livre) =
    match livre.Etat with
    | Emprunté ->
        // Mettre à jour l'état du livre à "Disponible"
        let livreRetourné = { livre with Etat = Disponible; DateRetour = DateTime.Now }
        // Supprimer le livre de la liste des livres empruntés par le lecteur
        let lecteurSansLivre = { lecteur with LivresEmpruntés = lecteur.LivresEmpruntés |> List.filter (fun l -> l <> livre) }
        // Remplacer le livre dans la liste des livres de la bibliothèque
        [livreRetourné] |> List.replace livres livre
        // Retourner le lecteur sans le livre retourné
        lecteurSansLivre
    | _ -> failwith "Le livre n'est pas emprunté"

// Générer un rapport sur les livres empruntés par un lecteur
let générerRapportEmprunt(lecteur: Lecteur) =
    printfn "\nRapport d'Emprunt pour %s %s" lecteur.Nom lecteur.Prenom
    printfn "-------------------------"
    lecteur.LivresEmpruntés |> Seq.iter (fun l -> printf "%s %s (%s)\n" l.Titre l.Auteur l.DateEmprunt)

// Exemple d'utilisation

// Créer une liste de livres
let livres =
    [
        { Titre = "Anna Karénine"; Auteur = "Léon Tolstoï"; Genre = Roman; Etat = Disponible }
        { Titre = "1984"; Auteur = "George Orwell"; Genre = ScienceFiction; Etat = Disponible }
        { Titre = "La Vie de Gandhi"; Auteur = "Mahatma Gandhi"; Genre = Biographie; Etat = Disponible }
    ]

// Créer un lecteur
let lecteur =
    {
        Nom = "Dupont"
        Prenom = "Jean"
        Email = "jean.dupont@example.com"
        Téléphone = "0123456789"
        Adresse = "1 rue de la Paix"
        LivresEmpruntés = []
    }

// Ajouter un livre à la bibliothèque
livres <- ajouterLivre livres { Titre = "Les Misérables"; Auteur = "Victor Hugo"; Genre = Roman; Etat = Disponible }

// Emprunter un livre par le lecteur
lecteur <- emprunterLivre livres lecteur (rechercherLivreParTitre livres "1984") |> ignore

// Retourner le livre par le lecteur
lecteur <- retournerLivre livres lecteur (rechercherLivreParTitre livres "1984") |> ignore

// Générer le rapport d'emprunt
générerRapportEmprunt lecteur
```

**Explication du Code**

Ce code met en œuvre un système de gestion de bibliothèque en F#. Il permet d'ajouter des livres à la bibliothèque, de les rechercher par titre, de les emprunter et de les retourner par des lecteurs.

* **Enums et Types:** Des enums sont utilisés pour représenter les genres de livres et les états des livres. Des types sont définis pour représenter les livres et les lecteurs.
* **Fonctions:** Des fonctions sont définies pour effectuer différentes opérations sur les livres et les lecteurs, comme l'ajout de livres, la recherche de livres, l'emprunt et le retour de livres, et la génération de rapports d'emprunt.
* **Exemple d'utilisation:** Un exemple d'utilisation est fourni pour illustrer comment utiliser les fonctions pour gérer une bibliothèque. Il crée une liste de livres, un lecteur, ajoute un livre, emprunte un livre, le retourne et génère un rapport d'emprunt.