```dart
// Cette classe représente un système de coordonnées à deux dimensions.
class Point2D {
  late double x, y; // Les coordonnées x et y du point.

  // Constructeur par défaut qui initialise les coordonnées à 0.
  Point2D() : x = 0.0, y = 0.0 {}

  // Constructeur qui prend les coordonnées en paramètre.
  Point2D.from coordonnées(double x, double y)
      : x = x,
        y = y {}

  // Getter pour la coordonnée x.
  double get abscisse => x;

  // Setter pour la coordonnée x.
  set abscisse(double value) => x = value;

  // Getter pour la coordonnée y.
  double get ordonnée => y;

  // Setter pour la coordonnée y.
  set ordonnée(double value) => y = value;

  // Méthode pour déplacer le point.
  void déplacer(double dx, double dy) {
    x += dx;
    y += dy;
  }

  // Méthode pour calculer la distance entre deux points.
  double distanceDepuis(Point2D autre) {
    return math.sqrt(math.pow(x - autre.x, 2) + math.pow(y - autre.y, 2));
  }

  // Méthode pour retourner une représentation sous forme de chaîne de caractères.
  @override
  String toString() {
    return 'Point2D(x: $x, y: $y)';
  }
}

// Cette classe représente un cercle.
class Cercle {
  late Point2D centre; // Le centre du cercle.
  late double rayon; // Le rayon du cercle.

  // Constructeur par défaut qui initialise le centre au point (0, 0) et le rayon à 1.
  Cercle()
      : centre = Point2D(),
        rayon = 1.0 {}

  // Constructeur qui prend le centre et le rayon en paramètre.
  Cercle.from centreEtRayon(Point2D centre, double rayon)
      : centre = centre,
        rayon = rayon {}

  // Getter pour le centre.
  Point2D get pointCentral => centre;

  // Setter pour le centre.
  set pointCentral(Point2D value) => centre = value;

  // Getter pour le rayon.
  double get valuation => rayon;

  // Setter pour le rayon.
  set valuation(double value) => rayon = value;

  // Méthode pour calculer la circonférence.
  double circonférence() {
    return 2 * math.pi * rayon;
  }

  // Méthode pour calculer l'aire.
  double aire() {
    return math.pi * rayon * rayon;
  }

  // Méthode pour retourner une représentation sous forme de chaîne de caractères.
  @override
  String toString() {
    return 'Cercle(centre: $centre, rayon: $rayon)';
  }
}

// Cette classe représente un triangle.
class Triangle {
  late Point2D sommetA;
  late Point2D sommetB;
  late Point2D sommetC;

  // Constructeur par défaut qui initialise les sommets au point (0, 0).
  Triangle()
      : sommetA = Point2D(),
        sommetB = Point2D(),
        sommetC = Point2D() {}

  // Constructeur qui prend les sommets en paramètre.
  Triangle.from sommets(Point2D sommetA, Point2D sommetB, Point2D sommetC)
      : sommetA = sommetA,
        sommetB = sommetB,
        sommetC = sommetC {}

  // Getter pour le sommet A.
  Point2D get sommetUn => sommetA;

  // Setter pour le sommet A.
  set sommetUn(Point2D value) => sommetA = value;

  // Getter pour le sommet B.
  Point2D get sommetDeux => sommetB;

  // Setter pour le sommet B.
  set sommetDeux(Point2D value) => sommetB = value;

  // Getter pour le sommet C.
  Point2D get sommetTrois => sommetC;

  // Setter pour le sommet C.
  set sommetTrois(Point2D value) => sommetC = value;

  // Méthode pour calculer le périmètre.
  double périmètre() {
    return sommetA.distanceDepuis(sommetB) +
        sommetB.distanceDepuis(sommetC) +
        sommetC.distanceDepuis(sommetA);
  }

  // Méthode pour calculer l'aire.
  double aire() {
    // Utilisation de la formule de Héron.
    double semiPérimètre = périmètre() / 2;
    return math.sqrt(
        semiPérimètre *
            (semiPérimètre - sommetA.distanceDepuis(sommetB)) *
            (semiPérimètre - sommetB.distanceDepuis(sommetC)) *
            (semiPérimètre - sommetC.distanceDepuis(sommetA)));
  }

  // Méthode pour retourner une représentation sous forme de chaîne de caractères.
  @override
  String toString() {
    return 'Triangle' +
        '(sommetA: $sommetA, sommetB: $sommetB, sommetC: $sommetC)';
  }
}

// Fonction principale du programme.
void main() {
  // Création d'un point.
  var point = Point2D();

  // Modification des coordonnées du point.
  point.abscisse = 3.14;
  point.ordonnée = 2.71;

  // Affichage des coordonnées du point.
  print('Les coordonnées du point sont : ${point.abscisse}, ${point.ordonnée}');

  // Création d'un cercle.
  var cercle = Cercle();

  // Modification du centre et du rayon du cercle.
  cercle.pointCentral = Point2D.from coordonnées(2.0, 1.0);
  cercle.valuation = 3.0;

  // Affichage des informations du cercle.
  print('Le centre du cercle est : ${cercle.pointCentral}');
  print('Le rayon du cercle est : ${cercle.valuation}');

  // Création d'un triangle.
  var triangle = Triangle();

  // Modification des sommets du triangle.
  triangle.sommetUn = Point2D.from coordonnées(0.0, 0.0);
  triangle.sommetDeux = Point2D.from coordonnées(1.0, 0.0);
  triangle.sommetTrois = Point2D.from coordonnées(1.0, 2.0);

  // Affichage des informations du triangle.
  print('Les sommets du triangle sont : ${triangle.sommetUn}, ${triangle.sommetDeux}, ${triangle.sommetTrois}');
}
```

**Explication du code :**

Ce code est un programme Dart qui crée des objets représentant des points, des cercles et des triangles dans un système de coordonnées à deux dimensions. Il permet de modifier les propriétés de ces objets et de calculer leur périmètre, leur aire et d'autres informations.

1. **Classe Point2D :** Représente un point dans un système de coordonnées à deux dimensions. Il possède des propriétés pour les coordonnées x et y, ainsi que des méthodes pour les récupérer et les modifier.

2. **Classe Cercle :** Représente un cercle. Il possède des propriétés pour le centre et le rayon du cercle, ainsi que des méthodes pour calculer la circonférence et l'aire.

3. **Classe Triangle :** Représente un triangle. Il possède des propriétés pour les trois sommets du triangle, ainsi que des méthodes pour calculer le périmètre et l'aire.

4. **Fonction main :** C'est le point d'entrée du programme. Elle crée des instances de ces classes, modifie leurs propriétés et affiche les informations sur ces objets.