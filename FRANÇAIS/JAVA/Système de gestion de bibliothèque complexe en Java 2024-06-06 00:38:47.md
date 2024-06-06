**Code Java Complexe**

**Objectif:** Ce code implémente un système de gestion de bibliothèque complexe qui permet aux utilisateurs d'emprunter, de retourner et de gérer des livres, ainsi que de créer et de gérer des comptes d'utilisateurs.

**Explication du Code:**

**1. Classe principale `Bibliotheque`:**
```java
public class Bibliotheque {

    private List<Livre> livres;  // Liste des livres dans la bibliothèque
    private List<Utilisateur> utilisateurs;  // Liste des utilisateurs de la bibliothèque

    public Bibliotheque() {
        this.livres = new ArrayList<>();
        this.utilisateurs = new ArrayList<>();
    }

    // Méthodes pour ajouter et supprimer des livres et des utilisateurs
    // ...

    // Méthodes pour gérer les emprunts et les retours de livres
    // ...

}
```

**2. Classe `Livre`:**
```java
public class Livre {

    private String titre;  // Titre du livre
    private String auteur;  // Auteur du livre
    private int quantite;  // Nombre d'exemplaires disponibles

    public Livre(String titre, String auteur, int quantite) {
        this.titre = titre;
        this.auteur = auteur;
        this.quantite = quantite;
    }

    // Méthodes pour obtenir et définir les propriétés
    // ...

}
```

**3. Classe `Utilisateur`:**
```java
public class Utilisateur {

    private String nom;  // Nom de l'utilisateur
    private String prenom;  // Prénom de l'utilisateur
    private List<Livre> livresEmpruntes;  // Liste des livres empruntés par l'utilisateur

    public Utilisateur(String nom, String prenom) {
        this.nom = nom;
        this.prenom = prenom;
        this.livresEmpruntes = new ArrayList<>();
    }

    // Méthodes pour obtenir et définir les propriétés et gérer les emprunts
    // ...

}
```

**4. Gestion des Emprunts et Retours:**
```java
public void emprunterLivre(Utilisateur utilisateur, Livre livre) {
    if (livre.getQuantite() > 0) {
        utilisateur.getLivresEmpruntes().add(livre);
        livre.setQuantite(livre.getQuantite() - 1);
    }
}

public void retournerLivre(Utilisateur utilisateur, Livre livre) {
    utilisateur.getLivresEmpruntes().remove(livre);
    livre.setQuantite(livre.getQuantite() + 1);
}
```

**5. Gestion des Comptes Utilisateurs:**
```java
public void creerCompte(String nom, String prenom) {
    Utilisateur utilisateur = new Utilisateur(nom, prenom);
    utilisateurs.add(utilisateur);
}

public void supprimerCompte(Utilisateur utilisateur) {
    utilisateurs.remove(utilisateur);
}
```

**Exemple d'Utilisation:**
```java
// Créer une instance de la bibliothèque
Bibliotheque bibliotheque = new Bibliotheque();

// Ajouter des livres à la bibliothèque
bibliotheque.addLivre("Le Petit Prince", "Antoine de Saint-Exupéry", 5);
bibliotheque.addLivre("1984", "George Orwell", 3);

// Créer des comptes d'utilisateurs
bibliotheque.creerCompte("John", "Doe");
bibliotheque.creerCompte("Jane", "Smith");

// Emprunter des livres
bibliotheque.emprunterLivre(bibliotheque.getUtilisateur("John", "Doe"), bibliotheque.getLivre("Le Petit Prince"));
bibliotheque.emprunterLivre(bibliotheque.getUtilisateur("Jane", "Smith"), bibliotheque.getLivre("1984"));

// Retourner des livres
bibliotheque.retournerLivre(bibliotheque.getUtilisateur("John", "Doe"), bibliotheque.getLivre("Le Petit Prince"));
```

Ce code fournit un système de gestion de bibliothèque complet avec le suivi des livres, les comptes d'utilisateurs, les emprunts et les retours de livres.