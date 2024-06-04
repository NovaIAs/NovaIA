**Code complexe en Java**

**Fonctionnalité:** Ce code met en œuvre un système de gestion de bibliothèque qui permet aux utilisateurs de gérer les livres, les emprunts et les membres.

```java
import java.util.*;

public class GestionBibliotheque {

    private List<Livre> livres;
    private List<Membre> membres;
    private List<Emprunt> emprunts;

    public GestionBibliotheque() {
        livres = new ArrayList<>();
        membres = new ArrayList<>();
        emprunts = new ArrayList<>();
    }

    public void ajouterLivre(Livre livre) {
        livres.add(livre);
    }

    public void ajouterMembre(Membre membre) {
        membres.add(membre);
    }

    public void enregistrerEmprunt(Livre livre, Membre membre) {
        Emprunt emprunt = new Emprunt(livre, membre);
        emprunts.add(emprunt);
    }

    public void retournerLivre(Livre livre, Membre membre) {
        Emprunt emprunt = trouverEmprunt(livre, membre);
        emprunt.setDateRetour(new Date());
    }

    public List<Livre> rechercherLivres(String titre) {
        List<Livre> livresTrouves = new ArrayList<>();
        for (Livre livre : livres) {
            if (livre.getTitre().contains(titre)) {
                livresTrouves.add(livre);
            }
        }
        return livresTrouves;
    }

    public List<Membre> rechercherMembres(String nom) {
        List<Membre> membresTrouves = new ArrayList<>();
        for (Membre membre : membres) {
            if (membre.getNom().contains(nom)) {
                membresTrouves.add(membre);
            }
        }
        return membresTrouves;
    }

    public List<Emprunt> rechercherEmprunts(Livre livre, Membre membre) {
        List<Emprunt> empruntsTrouves = new ArrayList<>();
        for (Emprunt emprunt : emprunts) {
            if (emprunt.getLivre().equals(livre) && emprunt.getMembre().equals(membre)) {
                empruntsTrouves.add(emprunt);
            }
        }
        return empruntsTrouves;
    }

    private Emprunt trouverEmprunt(Livre livre, Membre membre) {
        for (Emprunt emprunt : emprunts) {
            if (emprunt.getLivre().equals(livre) && emprunt.getMembre().equals(membre)) {
                return emprunt;
            }
        }
        return null;
    }

    // ... Autres méthodes ...
}

class Livre {

    private String titre;
    private String auteur;
    private int isbn;

    public Livre(String titre, String auteur, int isbn) {
        this.titre = titre;
        this.auteur = auteur;
        this.isbn = isbn;
    }

    // ... Getters et setters ...
}

class Membre {

    private String nom;
    private String prenom;
    private String adresse;

    public Membre(String nom, String prenom, String adresse) {
        this.nom = nom;
        this.prenom = prenom;
        this.adresse = adresse;
    }

    // ... Getters et setters ...
}

class Emprunt {

    private Livre livre;
    private Membre membre;
    private Date dateEmprunt;
    private Date dateRetour;

    public Emprunt(Livre livre, Membre membre) {
        this.livre = livre;
        this.membre = membre;
        dateEmprunt = new Date();
    }

    // ... Getters et setters ...
}
```

**Explication du code:**

Ce code implémente un système de gestion de bibliothèque en utilisant des classes et des collections. Les classes représentent les différentes entités du système : livres, membres et emprunts. Les collections sont utilisées pour stocker les instances de ces classes.

Les méthodes de la classe `GestionBibliotheque` permettent de gérer les livres, les membres et les emprunts. Les méthodes de recherche permettent de rechercher des livres et des membres par leur titre ou leur nom, respectivement. Les méthodes d'emprunt et de retour permettent d'enregistrer et de retourner des livres empruntés.

La classe `Livre` représente un livre avec son titre, son auteur et son ISBN. La classe `Membre` représente un membre avec son nom, son prénom et son adresse. La classe `Emprunt` représente un emprunt avec le livre emprunté, le membre qui l'emprunte, la date d'emprunt et la date de retour.

Ce code fournit une base solide pour un système de gestion de bibliothèque fonctionnel. Il peut être étendu avec des fonctionnalités supplémentaires, telles que la gestion des retards, les notifications par e-mail et la génération de rapports.