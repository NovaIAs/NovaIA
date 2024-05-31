**Assistant d'achat d'articles ménagers virtuel**

```dart
import 'dart:io';

class Article {
  String nom;
  double prix;
  int quantite;

  Article(this.nom, this.prix, this.quantite);

  @override
  String toString() => "$nom : $prix € ($quantite)";
}

class Panier {
  List<Article> articles = [];

  void ajouter(Article article) => articles.add(article);

  double get total => articles.fold(0, (total, article) => total + article.prix * article.quantite);

  @override
  String toString() => "Panier : ${articles.join(', ')}";
}

class Assistant {
  Map<String, Article> catalogue = {};
  Panier panier = Panier();

  Assistant() {
    catalogue["Cafetière"] = Article("Cafetière", 25.0, 1);
    catalogue["Machine à café"] = Article("Machine à café", 100.0, 1);
    catalogue["Bouilloire"] = Article("Bouilloire", 30.0, 1);
    catalogue["Grille-pain"] = Article("Grille-pain", 40.0, 1);
  }

  void afficherCatalogue() {
    print("Catalogue :");
    for (var article in catalogue.values) {
      print(article);
    }
  }

  void ajouterAuPanier(String nom, int quantite) {
    if (catalogue.containsKey(nom)) {
      Article article = catalogue[nom]!;
      article.quantite += quantite;
      panier.ajouter(article);
    } else {
      print("Article non trouvé dans le catalogue.");
    }
  }

  void afficherPanier() {
    print("Panier :");
    print(panier);
    print("Total : ${panier.total} €");
  }

  void commander() {
    print("Votre commande a été passée.");
  }

  void lancer() {
    int choix;
    do {
      print("1. Afficher le catalogue");
      print("2. Ajouter un article au panier");
      print("3. Afficher le panier");
      print("4. Commander");
      print("5. Quitter");
      print("Votre choix : ");
      choix = int.parse(stdin.readLineSync() ?? "0");

      switch (choix) {
        case 1:
          afficherCatalogue();
          break;
        case 2:
          print("Nom de l'article à ajouter : ");
          String nom = stdin.readLineSync() ?? "";
          print("Quantité à ajouter : ");
          int quantite = int.parse(stdin.readLineSync() ?? "0");
          ajouterAuPanier(nom, quantite);
          break;
        case 3:
          afficherPanier();
          break;
        case 4:
          commander();
          break;
        case 5:
          print("Au revoir !");
          break;
        default:
          print("Choix invalide.");
      }
    } while (choix != 5);
  }
}

void main() {
  Assistant assistant = Assistant();
  assistant.lancer();
}
```

**Explication du code :**

* **Classes Article, Panier et Assistant** : Ces classes modélisent les concepts d'articles ménagers, de panier d'achat et d'assistant virtuel, respectivement.

* **Catalogue** : Un dictionnaire (Map) contenant les articles disponibles dans le catalogue, avec leurs noms comme clés.

* **Panier** : Une liste d'articles représentant les articles ajoutés au panier par l'utilisateur.

* **Assistant** : Cette classe gère l'interaction avec l'utilisateur, l'accès au catalogue et les opérations sur le panier.

* **Méthodes** :

    * **afficherCatalogue** : Affiche tous les articles du catalogue.
    * **ajouterAuPanier** : Ajoute un article au panier, en ajustant sa quantité si l'article est déjà présent.
    * **afficherPanier** : Affiche les articles dans le panier et le total.
    * **commander** : Simule la passation d'une commande.
    * **lancer** : Boucle principale qui permet à l'utilisateur d'interagir avec l'assistant et d'effectuer des actions.

* **Fonction main** : Crée une instance de l'assistant et lance l'interface utilisateur.