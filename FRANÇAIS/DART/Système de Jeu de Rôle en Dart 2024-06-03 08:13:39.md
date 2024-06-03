**Système de Jeu de Rôle**

**Classe Personnage**

```dart
class Personnage {
  String nom;
  int pointsDeVie;
  int pointsDAttaque;
  int pointsDeDefense;

  Personnage(this.nom, this.pointsDeVie, this.pointsDAttaque, this.pointsDeDefense);

  void attaquer(Personnage adversaire) {
    adversaire.pointsDeVie -= this.pointsDAttaque - adversaire.pointsDeDefense;
  }

  bool estMort() {
    return this.pointsDeVie <= 0;
  }
}
```

**Classe Combat**

```dart
class Combat {
  Personnage personnage1;
  Personnage personnage2;

  Combat(this.personnage1, this.personnage2);

  void demarrer() {
    while (!personnage1.estMort() && !personnage2.estMort()) {
      personnage1.attaquer(personnage2);
      personnage2.attaquer(personnage1);
    }

    if (personnage1.estMort()) {
      print("${personnage2.nom} a gagné !");
    } else {
      print("${personnage1.nom} a gagné !");
    }
  }
}
```

**Classe Inventaire**

```dart
class Inventaire {
  List<Objet> objets = [];

  void ajouterObjet(Objet objet) {
    objets.add(objet);
  }

  Objet getObjetParNom(String nom) {
    return objets.firstWhere((objet) => objet.nom == nom);
  }

  void utiliserObjet(Objet objet) {
    objet.utiliser();
  }
}
```

**Classe Objet**

```dart
class Objet {
  String nom;
  String description;

  Objet(this.nom, this.description);

  void utiliser() {
    print("L'objet $nom a été utilisé.");
  }
}
```

**Classe Potion**

```dart
class Potion extends Objet {
  int pointsDeVie;

  Potion(String nom, String description, this.pointsDeVie) : super(nom, description);

  @override
  void utiliser() {
    super.utiliser();
    personnage.pointsDeVie += pointsDeVie;
  }
}
```

**Classe Epee**

```dart
class Epee extends Objet {
  int pointsDAttaque;

  Epee(String nom, String description, this.pointsDAttaque) : super(nom, description);

  @override
  void utiliser() {
    super.utiliser();
    personnage.pointsDAttaque += pointsDAttaque;
  }
}
```

**Exemple d'utilisation**

```dart
// Création des personnages
Personnage guerrier = Personnage("Guerrier", 100, 20, 10);
Personnage mage = Personnage("Mage", 80, 15, 15);

// Création du combat
Combat combat = Combat(guerrier, mage);

// Création de l'inventaire
Inventaire inventaire = Inventaire();

// Ajout d'objets à l'inventaire
inventaire.ajouterObjet(Potion("Potion de soin", "Restaure 20 points de vie", 20));
inventaire.ajouterObjet(Epee("Épée tranchante", "Augmente les points d'attaque de 10", 10));

// Démarrage du combat
combat.demarrer();

// Utilisation d'un objet de l'inventaire
inventaire.getObjetParNom("Potion de soin").utiliser();
```

**Explication du code**

Ce code implémente un système de jeu de rôle simple en Dart.

La **classe Personnage** représente les personnages du jeu, avec leurs points de vie, d'attaque et de défense. La **classe Combat** gère les combats entre les personnages, en vérifiant constamment si les personnages sont vivants et en échangeant des attaques.

**La classe Inventaire** stocke les objets que les personnages peuvent utiliser, tels que les potions et les armes. La **classe Objet** est la classe de base pour tous les objets, et les **classes Potion** et **Epee** sont des classes dérivées qui fournissent des capacités spécifiques.

La **méthode principale** crée deux personnages, un combat et un inventaire. Elle ajoute des objets à l'inventaire et lance le combat. Enfin, elle montre l'utilisation d'un objet de l'inventaire pour restaurer les points de vie d'un personnage.