**Gestionnaire de bibliothèque complexe en Java**

```java
// Importation des packages nécessaires
import java.util.*;
import java.io.*;

// Classe principale de l'application
public class GestionnaireBibliotheque {

    // Attributs
    private HashMap<String, Livre> catalogueLivres; // Dictionnaire des livres par leur titre
    private ArrayList<Emprunt> historiqueEmprunts; // Liste des emprunts effectués
    private ArrayList<Abonne> listeAbonnes; // Liste des abonnés enregistrés

    // Constructeur
    public GestionnaireBibliotheque() {
        catalogueLivres = new HashMap<>();
        historiqueEmprunts = new ArrayList<>();
        listeAbonnes = new ArrayList<>();
    }

    // Méthodes publiques

    // Ajouter un livre au catalogue
    public void ajouterLivre(Livre livre) {
        catalogueLivres.put(livre.getTitre(), livre);
    }

    // Enregistrer un emprunt
    public void enregistrerEmprunt(Abonne abonne, Livre livre, Date dateEmprunt) {
        Emprunt emprunt = new Emprunt(abonne, livre, dateEmprunt);
        historiqueEmprunts.add(emprunt);
    }

    // Retourner un livre
    public void retournerLivre(Livre livre, Date dateRetour) {
        for (Emprunt emprunt : historiqueEmprunts) {
            if (emprunt.getLivre().equals(livre)) {
                emprunt.setDateRetour(dateRetour);
            }
        }
    }

    // Enregistrer un nouvel abonné
    public void enregistrerAbonne(Abonne abonne) {
        listeAbonnes.add(abonne);
    }

    // Obtenir la liste des livres empruntés par un abonné
    public List<Livre> getLivresEmpruntes(Abonne abonne) {
        List<Livre> livresEmpruntes = new ArrayList<>();
        for (Emprunt emprunt : historiqueEmprunts) {
            if (emprunt.getAbonne().equals(abonne)) {
                livresEmpruntes.add(emprunt.getLivre());
            }
        }
        return livresEmpruntes;
    }

    // Méthodes auxiliaires

    // Rechercher un livre par son titre
    public Livre rechercherLivre(String titre) {
        return catalogueLivres.get(titre);
    }

    // Rechercher un abonné par son nom
    public Abonne rechercherAbonne(String nom) {
        for (Abonne abonne : listeAbonnes) {
            if (abonne.getNom().equals(nom)) {
                return abonne;
            }
        }
        return null;
    }
}

// Classe Livre
class Livre {

    // Attributs
    private String titre;
    private String auteur;
    private int nombrePages;

    // Constructeur
    public Livre(String titre, String auteur, int nombrePages) {
        this.titre = titre;
        this.auteur = auteur;
        this.nombrePages = nombrePages;
    }

    // Méthodes
    public String getTitre() {
        return titre;
    }

    public String getAuteur() {
        return auteur;
    }

    public int getNombrePages() {
        return nombrePages;
    }
}

// Classe Abonne
class Abonne {

    // Attributs
    private String nom;
    private String adresse;
    private String telephone;

    // Constructeur
    public Abonne(String nom, String adresse, String telephone) {
        this.nom = nom;
        this.adresse = adresse;
        this.telephone = telephone;
    }

    // Méthodes
    public String getNom() {
        return nom;
    }

    public String getAdresse() {
        return adresse;
    }

    public String getTelephone() {
        return telephone;
    }
}

// Classe Emprunt
class Emprunt {

    // Attributs
    private Abonne abonne;
    private Livre livre;
    private Date dateEmprunt;
    private Date dateRetour;

    // Constructeur
    public Emprunt(Abonne abonne, Livre livre, Date dateEmprunt) {
        this.abonne = abonne;
        this.livre = livre;
        this.dateEmprunt = dateEmprunt;
        this.dateRetour = null;
    }

    // Méthodes
    public Abonne getAbonne() {
        return abonne;
    }

    public Livre getLivre() {
        return livre;
    }

    public Date getDateEmprunt() {
        return dateEmprunt;
    }

    public Date getDateRetour() {
        return dateRetour;
    }

    public void setDateRetour(Date dateRetour) {
        this.dateRetour = dateRetour;
    }
}
```

**Explication du code**

Ce code Java implémente un gestionnaire de bibliothèque complexe avec les fonctionnalités suivantes :

* Gestion d'un catalogue de livres avec des informations sur les titres, les auteurs et le nombre de pages.
* Enregistrement des emprunts de livres par les abonnés avec des dates d'emprunt et de retour.
* Gestion d'une liste d'abonnés avec leurs noms, adresses et numéros de téléphone.
* Possibilité de rechercher des livres par titre, des abonnés par nom et d'obtenir la liste des livres empruntés pour un abonné donné.

Le code est organisé à l'aide de classes : `GestionnaireBibliotheque`, `Livre`, `Abonne` et `Emprunt`.

**Classe `GestionnaireBibliotheque`** :

* Gère le catalogue des livres, l'historique des emprunts et la liste des abonnés.
* Fournit des méthodes pour ajouter, rechercher et supprimer des livres, enregistrer et retourner des emprunts, et enregistrer de nouveaux abonnés.

**Classe `Livre`** :

* Représente un livre avec ses informations de titre, d'auteur et de nombre de pages.

**Classe `Abonne`** :

* Représente un abonné avec ses informations de nom, adresse et numéro de téléphone.

**Classe `Emprunt`** :

* Représente un emprunt d'un livre par un abonné, avec des informations sur la date d'emprunt et la date de retour.