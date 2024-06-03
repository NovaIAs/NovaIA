**Objectif:** Créer un système de gestion de bibliothèque complexe en PHP.

**Code:**

```php
<?php

// Classe représentant une bibliothèque
class Bibliotheque
{
    private $livres;  // Tableau des livres
    private $membres; // Tableau des membres
    private $emprunts; // Tableau des emprunts

    // Constructeur
    public function __construct()
    {
        $this->livres = [];
        $this->membres = [];
        $this->emprunts = [];
    }

    // Ajouter un livre
    public function ajouterLivre(Livre $livre)
    {
        $this->livres[] = $livre;
    }

    // Ajouter un membre
    public function ajouterMembre(Membre $membre)
    {
        $this->membres[] = $membre;
    }

    // Enregistrer un emprunt
    public function enregistrerEmprunt(Emprunt $emprunt)
    {
        $this->emprunts[] = $emprunt;
    }

    // Obtenir les livres disponibles
    public function getLivresDisponibles()
    {
        $livresDisponibles = [];
        foreach ($this->livres as $livre) {
            if (!$this->estEmprunte($livre)) {
                $livresDisponibles[] = $livre;
            }
        }
        return $livresDisponibles;
    }

    // Vérifier si un livre est emprunté
    public function estEmprunte(Livre $livre)
    {
        foreach ($this->emprunts as $emprunt) {
            if ($emprunt->getLivre() === $livre && !$emprunt->estRendu()) {
                return true;
            }
        }
        return false;
    }
}

// Classe représentant un livre
class Livre
{
    private $titre;  // Titre du livre
    private $auteur; // Auteur du livre
    private $isbn;   // ISBN du livre

    // Constructeur
    public function __construct($titre, $auteur, $isbn)
    {
        $this->titre = $titre;
        $this->auteur = $auteur;
        $this->isbn = $isbn;
    }

    // Obtenir le titre
    public function getTitre()
    {
        return $this->titre;
    }

    // Obtenir l'auteur
    public function getAuteur()
    {
        return $this->auteur;
    }

    // Obtenir l'ISBN
    public function getIsbn()
    {
        return $this->isbn;
    }
}

// Classe représentant un membre
class Membre
{
    private $nom;      // Nom du membre
    private $prenom;   // Prénom du membre
    private $email;    // Email du membre
    private $adresse;  // Adresse du membre

    // Constructeur
    public function __construct($nom, $prenom, $email, $adresse)
    {
        $this->nom = $nom;
        $this->prenom = $prenom;
        $this->email = $email;
        $this->adresse = $adresse;
    }

    // Obtenir le nom
    public function getNom()
    {
        return $this->nom;
    }

    // Obtenir le prénom
    public function getPrenom()
    {
        return $this->prenom;
    }

    // Obtenir l'email
    public function getEmail()
    {
        return $this->email;
    }

    // Obtenir l'adresse
    public function getAdresse()
    {
        return $this->adresse;
    }
}

// Classe représentant un emprunt
class Emprunt
{
    private $livre;   // Livre emprunté
    private $membre;  // Membre ayant emprunté le livre
    private $dateEmprunt; // Date d'emprunt
    private $dateRendu;  // Date de rendu (null si non rendu)

    // Constructeur
    public function __construct(Livre $livre, Membre $membre, $dateEmprunt)
    {
        $this->livre = $livre;
        $this->membre = $membre;
        $this->dateEmprunt = $dateEmprunt;
        $this->dateRendu = null;
    }

    // Obtenir le livre
    public function getLivre()
    {
        return $this->livre;
    }

    // Obtenir le membre
    public function getMembre()
    {
        return $this->membre;
    }

    // Obtenir la date d'emprunt
    public function getDateEmprunt()
    {
        return $this->dateEmprunt;
    }

    // Vérifier si le livre est rendu
    public function estRendu()
    {
        return $this->dateRendu !== null;
    }

    // Rendre le livre
    public function rendreLivre(DateTime $dateRendu)
    {
        $this->dateRendu = $dateRendu;
    }
}

// Initialisation de la bibliothèque
$bibliotheque = new Bibliotheque();

// Ajout de quelques livres à la bibliothèque
$livres = [
    new Livre("Les Misérables", "Victor Hugo", "9780451419439"),
    new Livre("Cent ans de solitude", "Gabriel García Márquez", "9780307950883"),
    new Livre("1984", "George Orwell", "9780451524935"),
];
foreach ($livres as $livre) {
    $bibliotheque->ajouterLivre($livre);
}

// Ajout de quelques membres à la bibliothèque
$membres = [
    new Membre("Dupont", "Jean", "jean.dupont@email.com", "1 rue de la Paix"),
    new Membre("Martin", "Marie", "marie.martin@email.com", "2 rue des Fleurs"),
    new Membre("Durant", "Paul", "paul.durant@email.com", "3 rue des Champs"),
];
foreach ($membres as $membre) {
    $bibliotheque->ajouterMembre($membre);
}

// Enregistrement de quelques emprunts
$emprunts = [
    new Emprunt($livres[0], $membres[0], new DateTime('2023-01-01')),
    new Emprunt($livres[1], $membres[1], new DateTime('2023-01-15')),
    new Emprunt($livres[2], $membres[2], new DateTime('2023-02-01')),
];
foreach ($emprunts as $emprunt) {
    $bibliotheque->enregistrerEmprunt($emprunt);
}

// Afficher les livres disponibles
$livresDisponibles = $bibliotheque->getLivresDisponibles();
echo "Livres disponibles :<br>";
foreach ($livresDisponibles as $livre) {
    echo "- {$livre->getTitre()}<br>";
}
```

**Explication du code:**

* **Classes:** Le code définit des classes pour représenter la bibliothèque, les livres, les membres et les emprunts.
* **Bibliothèque:** La classe `Bibliotheque` gère la collection de livres, de membres et d'emprunts. Elle fournit des méthodes pour ajouter, récupérer et manipuler ces entités.
* **Livres:** La classe `Livre` représente un livre avec son titre, son auteur et son ISBN.
* **Membres:** La classe `Membre` représente un membre avec son nom, son prénom, son email et son adresse.
* **Emprunts:** La classe `Emprunt` représente un emprunt d'un livre par un membre, avec des informations sur la date d'emprunt et la date de rendu (le cas échéant).
* **Manipulation des entités:** Le code initialise une instance de `Bibliotheque` et y ajoute des instances de `Livre`, `Membre` et `Emprunt`.
* **Livres disponibles:** Le code récupère et affiche les livres qui ne sont pas empruntés.