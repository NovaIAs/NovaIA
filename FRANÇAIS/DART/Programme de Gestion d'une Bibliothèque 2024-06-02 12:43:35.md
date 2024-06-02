**Programme de Gestion d'un Système de Bibliothèque**

```dart
import 'dart:io';

class Livre {
  String titre;
  String auteur;
  bool disponible;

  Livre(this.titre, this.auteur, this.disponible);

  @override
  String toString() {
    return "Titre: $titre, Auteur: $auteur, Disponible: $disponible";
  }
}

class Bibliotheque {
  List<Livre> livres;

  Bibliotheque() {
    livres = [];
  }

  void ajouterLivre(Livre livre) {
    livres.add(livre);
  }

  void supprimerLivre(Livre livre) {
    livres.remove(livre);
  }

  List<Livre> rechercherLivresParTitre(String titre) {
    return livres.where((livre) => livre.titre.contains(titre)).toList();
  }

  List<Livre> rechercherLivresParAuteur(String auteur) {
    return livres.where((livre) => livre.auteur.contains(auteur)).toList();
  }

  bool verifierDisponibilite(Livre livre) {
    return livre.disponible;
  }

  void emprunterLivre(Livre livre) {
    if (verifierDisponibilite(livre)) {
      livre.disponible = false;
    }
  }

  void rendreLivre(Livre livre) {
    livre.disponible = true;
  }
}

void main() {
  Bibliotheque bibliotheque = Bibliotheque();

  bibliotheque.ajouterLivre(Livre("Le Petit Prince", "Antoine de Saint-Exupéry", true));
  bibliotheque.ajouterLivre(Livre("Harry Potter à l'école des sorciers", "J.K. Rowling", false));
  bibliotheque.ajouterLivre(Livre("Le Seigneur des anneaux", "J.R.R. Tolkien", true));

  print("Livres disponibles dans la bibliothèque :");
  for (Livre livre in bibliotheque.livres) {
    print(livre);
  }

  print("\nRecherche de livres par titre :");
  String titreRecherche = "Harry";
  List<Livre> livresTrouves = bibliotheque.rechercherLivresParTitre(titreRecherche);
  for (Livre livre in livresTrouves) {
    print(livre);
  }

  print("\nRecherche de livres par auteur :");
  String auteurRecherche = "Tolkien";
  livresTrouves = bibliotheque.rechercherLivresParAuteur(auteurRecherche);
  for (Livre livre in livresTrouves) {
    print(livre);
  }

  print("\nEmprunt du livre \"Le Petit Prince\" :");
  Livre livreEmprunte = bibliotheque.livres.firstWhere((livre) => livre.titre == "Le Petit Prince");
  bibliotheque.emprunterLivre(livreEmprunte);

  print("\nLivres disponibles après emprunt :");
  for (Livre livre in bibliotheque.livres) {
    print(livre);
  }

  print("\nRendre le livre \"Le Petit Prince\" :");
  bibliotheque.rendreLivre(livreEmprunte);

  print("\nLivres disponibles après retour :");
  for (Livre livre in bibliotheque.livres) {
    print(livre);
  }
}
```

**Explication du Code :**

**Classes Livre et Bibliothèque :**

* **Livre** représente un livre avec un titre, un auteur et un statut de disponibilité.
* **Bibliothèque** est une collection de livres et fournit des méthodes pour ajouter, supprimer, rechercher et modifier des livres.

**Fonctions de Recherche :**

* **rechercherLivresParTitre** et **rechercherLivresParAuteur** recherchent et renvoient les livres correspondant aux critères de recherche.

**Fonctions de Gestion de Disponibilité :**

* **verifierDisponibilite** vérifie si un livre est disponible.
* **emprunterLivre** emprunte un livre en définissant sa disponibilité sur "faux".
* **rendreLivre** rend le livre et le marque comme disponible.

**Fonction Principale (main) :**

* Crée une instance de Bibliothèque et y ajoute des livres.
* Affiche la liste des livres disponibles.
* Recherche et affiche les livres par titre et par auteur.
* Emprunte un livre et affiche la liste des livres disponibles après l'emprunt.
* Rend le livre et affiche la liste des livres disponibles après le retour.