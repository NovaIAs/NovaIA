**Classe abstraite de forme géométrique**

```dart
abstract class FormeGeometrique {
  double surface();
  double perimetre();
  String type();
}
```

**Classe concrète de carré**

```dart
class Carre extends FormeGeometrique {
  double cote;

  Carre(this.cote);

  @override
  double surface() {
    return cote * cote;
  }

  @override
  double perimetre() {
    return 4 * cote;
  }

  @override
  String type() {
    return "Carre";
  }
}
```

**Classe concrète de cercle**

```dart
class Cercle extends FormeGeometrique {
  double rayon;

  Cercle(this.rayon);

  @override
  double surface() {
    return Math.PI * rayon * rayon;
  }

  @override
  double perimetre() {
    return 2 * Math.PI * rayon;
  }

  @override
  String type() {
    return "Cercle";
  }
}

```

**Classe concrète de rectangle**

```dart
class Rectangle extends FormeGeometrique {
  double longueur;
  double largeur;

  Rectangle(this.longueur, this.largeur);

  @override
  double surface() {
    return longueur * largeur;
  }

  @override
  double perimetre() {
    return 2 * (longueur + largeur);
  }

  @override
  String type() {
    return "Rectangle";
  }
}
```

**Classe de calculatrice de forme**

```dart
class CalculatriceForme {
  List<FormeGeometrique> formes = [];

  void ajouterForme(FormeGeometrique forme) {
    formes.add(forme);
  }

  double surfaceTotale() {
    double surfaceTotale = 0;

    for (FormeGeometrique forme in formes) {
      surfaceTotale += forme.surface();
    }

    return surfaceTotale;
  }

  double perimetreTotal() {
    double perimetreTotal = 0;

    for (FormeGeometrique forme in formes) {
      perimetreTotal += forme.perimetre();
    }

    return perimetreTotal;
  }

  void afficherFormes() {
    for (FormeGeometrique forme in formes) {
      print("Type : ${forme.type()}");
      print("Surface : ${forme.surface()}");
      print("Périmètre : ${forme.perimetre()}\n");
    }
  }
}
```

**Utilisation**

```dart
void main() {
  Carre carre = Carre(5);
  Cercle cercle = Cercle(3);
  Rectangle rectangle = Rectangle(10, 5);

  CalculatriceForme calculatrice = CalculatriceForme();
  calculatrice.ajouterForme(carre);
  calculatrice.ajouterForme(cercle);
  calculatrice.ajouterForme(rectangle);

  print("Surface totale : ${calculatrice.surfaceTotale()}");
  print("Périmètre total : ${calculatrice.perimetreTotal()}\n");

  calculatrice.afficherFormes();
}
```

**Explication**

Ce code définit une hiérarchie de classes de formes géométriques avec une classe abstraite de base et des classes concrètes pour chaque type de forme (carré, cercle, rectangle). Il définit également une classe de calculatrice de formes qui peut calculer la surface et le périmètre totaux d'une liste de formes, et afficher les détails de chaque forme.