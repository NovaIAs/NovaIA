```dart
import 'dart:collection';
import 'dart:math';

// Classe définissant un Noeud dans un arbre de QuadTree
class QuadTreeNode<T> {
  final Rectangle<double> bounds;
  final T? value;
  late QuadTreeNode<T>? northEast;
  late QuadTreeNode<T>? northWest;
  late QuadTreeNode<T>? southEast;
  late QuadTreeNode<T>? southWest;

  QuadTreeNode(this.bounds, this.value);

  static QuadTreeNode<T>? buildTree<T>(Iterable<T> elements, Rectangle<double> bounds) {
    if (elements.isEmpty) {
      return null;
    }

    // Calculer les bornes de chaque quadrant
    final double width = bounds.width / 2;
    final double height = bounds.height / 2;
    final Rectangle<double> northEast = Rectangle<double>(bounds.left + width, bounds.top, width, height);
    final Rectangle<double> northWest = Rectangle<double>(bounds.left, bounds.top, width, height);
    final Rectangle<double> southEast = Rectangle<double>(bounds.left + width, bounds.top + height, width, height);
    final Rectangle<double> southWest = Rectangle<double>(bounds.left, bounds.top + height, width, height);

    // Créer le noeud racine
    final QuadTreeNode<T> root = QuadTreeNode<T>(bounds, null);

    // Insérer tous les éléments dans le QuadTree
    for (final T element in elements) {
      root.insert(element);
    }

    return root;
  }

  void insert(T element) {
    // Vérifier si l'élément appartient à ce noeud
    if (!bounds.containsPoint(element)) {
      throw ArgumentError("L'élément n'appartient pas aux bornes de ce noeud.");
    }

    // Si le noeud est une feuille (n'a pas d'enfants), définir la valeur
    if (northEast == null) {
      value = element;
      return;
    }

    // Sinon, insérer l'élément dans le quadrant correspondant
    if (northEast!.bounds.containsPoint(element)) {
      northEast!.insert(element);
    } else if (northWest!.bounds.containsPoint(element)) {
      northWest!.insert(element);
    } else if (southEast!.bounds.containsPoint(element)) {
      southEast!.insert(element);
    } else if (southWest!.bounds.containsPoint(element)) {
      southWest!.insert(element);
    }
  }

  // Rechercher un élément dans le QuadTree
  T? find(Rectangle<double> bounds) {
    // Vérifier si les bornes du QuadTree intersectent les bornes recherchées
    if (!this.bounds.intersects(bounds)) {
      return null;
    }

    // Si le noeud est une feuille, retourner sa valeur
    if (northEast == null) {
      return value;
    }

    // Sinon, rechercher l'élément dans les quadrants correspondants
    T? result;
    if (northEast!.bounds.intersects(bounds)) {
      result = northEast!.find(bounds);
    }
    if (northWest!.bounds.intersects(bounds)) {
      result = northWest!.find(bounds);
    }
    if (southEast!.bounds.intersects(bounds)) {
      result = southEast!.find(bounds);
    }
    if (southWest!.bounds.intersects(bounds)) {
      result = southWest!.find(bounds);
    }

    return result;
  }
}

// Classe définissant un ensemble de points dans un plan
class PointSet<T> {
  final QuadTreeNode<T>? root;
  final List<T> elements = [];

  PointSet(Iterable<T> points, Rectangle<double> bounds) {
    root = QuadTreeNode<T>.buildTree<T>(points, bounds);
    elements.addAll(points);
  }

  // Insérer un point dans l'ensemble
  void insert(T point) {
    if (root == null) {
      throw ArgumentError("L'ensemble ne peut pas être vide.");
    }

    root!.insert(point);
    elements.add(point);
  }

  // Rechercher un point dans l'ensemble
  T? find(Rectangle<double> bounds) {
    if (root == null) {
      throw ArgumentError("L'ensemble ne peut pas être vide.");
    }

    return root!.find(bounds);
  }

  // Supprimer un point de l'ensemble
  void remove(T point) {
    if (root == null) {
      throw ArgumentError("L'ensemble ne peut pas être vide.");
    }

    // Supprimer l'élément du QuadTree
    root!.value = null;

    // Supprimer l'élément de la liste
    elements.remove(point);

    // Reconstruire le QuadTree si nécessaire
    if (elements.isEmpty) {
      root = null;
    }
  }

  // Obtenir tous les points de l'ensemble
  List<T> getAll() {
    return elements;
  }
}

// Exemple d'utilisation
void main() {
  // Créer un ensemble de points aléatoires
  final Random random = Random();
  final bounds = Rectangle<double>(0, 0, 1000, 1000);
  final points = List<Point>.generate(100, (i) => Point(random.nextDouble() * bounds.width, random.nextDouble() * bounds.height));

  // Créer un ensemble de points
  final pointSet = PointSet<Point>(points, bounds);

  // Rechercher un point dans l'ensemble
  final searchedPoint = pointSet.find(Rectangle<double>(500, 500, 100, 100));
  print(searchedPoint);

  // Insérer un nouveau point dans l'ensemble
  pointSet.insert(Point(550, 550));

  // Supprimer un point de l'ensemble
  pointSet.remove(points[0]);

  // Obtenir tous les points de l'ensemble
  final allPoints = pointSet.getAll();
  print(allPoints);
}

// Classe définissant un point dans un plan
class Point {
  final double x;
  final double y;

  Point(this.x, this.y);
}

class Rectangle<T extends num> {
  final T left;
  final T top;
  final T width;
  final T height;

  Rectangle(this.left, this.top, this.width, this.height);

  T get right => left + width;

  T get bottom => top + height;

  bool containsPoint(T point) => point >= left && point < right && point >= top && point < bottom;

  bool intersects(Rectangle<T> other) => !(right < other.left || left > other.right || bottom < other.top || top > other.bottom);
}
```

**Explication du code :**

Ce code implémente un QuadTree, une structure de données qui divise un espace rectangulaire en quatre quadrants et stocke des données à l'intérieur. Il est utilisé pour rechercher efficacement des points dans un espace à deux dimensions.

**Classes principales :**

* **QuadTreeNode** : Représente un noeud dans le QuadTree. Chaque noeud a des bornes qui définissent la région de l'espace qu'il contient et une valeur optionnelle qui peut être associée à cette région.
* **PointSet** : Représente un ensemble de points dans un espace à deux dimensions. Il utilise un QuadTree interne pour stocker et rechercher des points efficacement.

**Principales méthodes :**

* **QuadTreeNode.buildTree** : Crée un QuadTree à partir d'une collection de points et de bornes initiales.
* **QuadTreeNode.insert** : Insère un point dans le QuadTree.
* **QuadTreeNode.find** : Recherche un point dans le QuadTree en fonction de bornes.
* **PointSet.insert** : Insère un point dans l'ensemble de points.
* **PointSet.find** : Recherche un point dans l'ensemble de points en fonction de bornes.
* **PointSet.remove** : Supprime un point de l'ensemble de points.
* **PointSet.getAll** : Renvoie tous les points de l'ensemble de points.

**Exemple d'utilisation :**

L'exemple d'utilisation crée un ensemble de points aléatoires, le stocke dans un PointSet et effectue des opérations de recherche, d'insertion et de suppression.

**Complexité et avantages :**

* Le QuadTree a une complexité de recherche de O(log n), où n est le nombre de points dans l'ensemble.
* Il permet une recherche efficace de points dans des espaces à deux dimensions.
* Il est utile pour des applications telles que