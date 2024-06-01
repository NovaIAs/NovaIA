**Système de gestion de bibliothèque**

```dart
import 'dart:collection';
import 'dart:io';

// Modèle de données pour les livres
class Livre {
  String titre;
  String auteur;
  int nombreDePages;
  bool estEmprunté;

  Livre(this.titre, this.auteur, this.nombreDePages, this.estEmprunté);
}

// Modèle de données pour les lecteurs
class Lecteur {
  String nom;
  String adresse;
  List<Livre> livresEmpruntés;

  Lecteur(this.nom, this.adresse, this.livresEmpruntés);
}

// Interface utilisateur
class InterfaceUtilisateur {
  static void affichageMenu() {
    print("Menu principal :");
    print("1. Ajouter un livre");
    print("2. Ajouter un lecteur");
    print("3. Emprunter un livre");
    print("4. Rendre un livre");
    print("5. Afficher les livres disponibles");
    print("6. Afficher les lecteurs empruntant des livres");
    print("7. Quitter");
  }

  static int entrerChoix() {
    int choix;
    do {
      print("Entrez votre choix (1-7) :");
      choix = int.parse(stdin.readLineSync() ?? '0');
    } while (choix < 1 || choix > 7);
    return choix;
  }
}

// Gestionnaire de bibliothèque
class GestionnaireBibliotheque {
  List<Livre> livres;
  List<Lecteur> lecteurs;

  GestionnaireBibliotheque() {
    this.livres = [];
    this.lecteurs = [];
  }

  // Ajout d'un livre
  void ajouterLivre(Livre livre) {
    this.livres.add(livre);
  }

  // Ajout d'un lecteur
  void ajouterLecteur(Lecteur lecteur) {
    this.lecteurs.add(lecteur);
  }

  // Emprunt d'un livre
  void emprunterLivre(Lecteur lecteur, Livre livre) {
    if (livre.estEmprunté) {
      print("Le livre '${livre.titre}' est déjà emprunté.");
    } else {
      lecteur.livresEmpruntés.add(livre);
      livre.estEmprunté = true;
      print("Le livre '${livre.titre}' a été emprunté par '${lecteur.nom}'.");
    }
  }

  // Rendre un livre
  void rendreLivre(Lecteur lecteur, Livre livre) {
    if (!livre.estEmprunté) {
      print("Le livre '${livre.titre}' n'est pas emprunté.");
    } else {
      lecteur.livresEmpruntés.remove(livre);
      livre.estEmprunté = false;
      print("Le livre '${livre.titre}' a été rendu par '${lecteur.nom}'.");
    }
  }

  // Affichage des livres disponibles
  void afficherLivresDisponibles() {
    print("\nLivres disponibles :");
    for (var livre in this.livres) {
      if (!livre.estEmprunté) {
        print(" - '${livre.titre}' de ${livre.auteur}");
      }
    }
  }

  // Affichage des lecteurs empruntant des livres
  void afficherLecteursEmpruntantDesLivres() {
    print("\nLecteurs empruntant des livres :");
    for (var lecteur in this.lecteurs) {
      if (lecteur.livresEmpruntés.isNotEmpty) {
        print(" - ${lecteur.nom} - Livres empruntés :");
        for (var livre in lecteur.livresEmpruntés) {
          print("  * '${livre.titre}'");
        }
      }
    }
  }
}

// Programme principal
void main() {
  GestionnaireBibliotheque bibliotheque = GestionnaireBibliotheque();

  // Chargement de données initiales
  bibliotheque.ajouterLivre(
      Livre("Le Petit Prince", "Antoine de Saint-Exupéry", 96, false));
  bibliotheque.ajouterLivre(
      Livre("Harry Potter à l'école des sorciers", "J.K. Rowling", 223, false));
  bibliotheque.ajouterLivre(Livre("Le Seigneur des anneaux", "J.R.R. Tolkien", 1207, false));

  bibliotheque.ajouterLecteur(Lecteur("Alice", "123 Main Street", []));
  bibliotheque.ajouterLecteur(Lecteur("Bob", "456 Elm Street", []));

  // Boucle d'exécution principale
  bool continuer = true;
  while (continuer) {
    InterfaceUtilisateur.affichageMenu();
    switch (InterfaceUtilisateur.entrerChoix()) {
      case 1:
        // Ajouter un livre
        print("Entrez le titre du livre :");
        String titre = stdin.readLineSync() ?? '';
        print("Entrez l'auteur du livre :");
        String auteur = stdin.readLineSync() ?? '';
        print("Entrez le nombre de pages du livre :");
        int nombreDePages = int.parse(stdin.readLineSync() ?? '0');
        bibliotheque.ajouterLivre(Livre(titre, auteur, nombreDePages, false));
        break;
      case 2:
        // Ajouter un lecteur
        print("Entrez le nom du lecteur :");
        String nom = stdin.readLineSync() ?? '';
        print("Entrez l'adresse du lecteur :");
        String adresse = stdin.readLineSync() ?? '';
        bibliotheque.ajouterLecteur(Lecteur(nom, adresse, []));
        break;
      case 3:
        // Emprunter un livre
        print("Entrez le nom du lecteur qui emprunte le livre :");
        String nomLecteur = stdin.readLineSync() ?? '';
        print("Entrez le titre du livre à emprunter :");
        String titreLivre = stdin.readLineSync() ?? '';
        Lecteur? lecteur = bibliotheque.lecteurs
            .firstWhere((lecteur) => lecteur.nom == nomLecteur, orElse: () => null);
        if (lecteur == null) {
          print("Lecteur non trouvé.");
        } else {
          Livre? livre = bibliotheque.livres
              .firstWhere((livre) => livre.titre == titreLivre, orElse: () => null);
          if (livre == null) {
            print("Livre non trouvé.");
          } else {
            bibliotheque.emprunterLivre(lecteur, livre);
          }
        }
        break;
      case 4:
        // Rendre un livre
        print("Entrez le nom du lecteur qui rend le livre :");
        String nomLecteur = stdin.readLineSync() ?? '';
        print("Entrez le titre du livre à rendre :");
        String titreLivre = stdin.readLineSync() ?? '';
        Lecteur? lecteur = bibliotheque.lecteurs
            .firstWhere((lecteur) => lecteur.nom == nomLecteur, orElse: () => null);
        if (lecteur == null) {
          print("Lecteur non trouvé.");
        } else {
          Livre? livre = bibliotheque.livres
              .firstWhere((livre) => livre.titre == titreLivre, orElse: () => null);
          if (livre == null) {
            print("Livre non trouvé.");
          } else {
            bibliotheque.rendreLivre(lecteur, livre);
          }
        }
        break;
      case 5:
        // Afficher les livres disponibles
        bibliotheque.afficherLivresDisponibles();
        break;
      case 6:
        // Afficher les lecteurs empruntant des livres
        bibliotheque.afficherLecteursEmpruntantDesLivres();
        break;
      case 7:
        // Quitter
        print("Au revoir !");
        continuer = false;
        break;
      default:
        // Choix invalide
        print("Choix invalide. Veuillez réessayer.");
    }
  }
}
```

**Voici une explication détaillée du code :**

**Modèles de données**

* Le modèle `Livre` représente les attributs d'un livre, tels que son titre, son auteur, son nombre de pages et son statut d'emprunt (`estEmprunté`).
* Le modèle `Lecteur` représente les attributs d'un lecteur, tels que son nom, son adresse et une liste des livres empruntés.

**Interface utilisateur**

* La classe `InterfaceUtilisateur` fournit les interactions avec l'utilisateur via la console.
* Elle affiche un menu de choix et gère la saisie de l'utilisateur.

**Gestionnaire de bibliothèque**

* La classe `GestionnaireBibliotheque` gère les opérations de la bibliothèque, telles que l'ajout de livres, de lecteurs, l'emprunt et le retour de livres.
* Elle maintient une liste de `Livre`s et une liste de `Lecteur`