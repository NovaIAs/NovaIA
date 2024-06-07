**Programme de simulation d'un système de particules en deux dimensions**

**Objectif :** Simuler le mouvement de particules dans un espace bidimensionnel avec des interactions élastiques.

**Code :**

```dart
import 'dart:math';

class Particule {
  double x, y; // Position
  double vx, vy; // Vitesse
  double m; // Masse
  double r; // Rayon

  Particule(this.x, this.y, this.vx, this.vy, this.m, this.r);

  void update(double dt) {
    // Mettre à jour la position
    x += vx * dt;
    y += vy * dt;

    // Vérifier les collisions avec les bords
    if (x < r || x > 1.0 - r) vx = -vx;
    if (y < r || y > 1.0 - r) vy = -vy;
  }

  void collide(Particule autre) {
    // Calculer la vector normal à la collision
    double dx = (autre.x - x) / sqrt(pow(autre.x - x, 2) + pow(autre.y - y, 2));
    double dy = (autre.y - y) / sqrt(pow(autre.x - x, 2) + pow(autre.y - y, 2));

    // Calculer les vitesses après la collision
    double newVx = vx - 2 * dot(dx, dy) * dx;
    double newVy = vy - 2 * dot(dx, dy) * dy;
    autre.vx = autre.vx - 2 * dot(dx, dy) * dx;
    autre.vy = autre.vy - 2 * dot(dx, dy) * dy;

    // Mettre à jour les vitesses
    vx = newVx;
    vy = newVy;
  }

  // Produit scalaire
  double dot(double x1, double y1, double x2, double y2) {
    return x1 * x2 + y1 * y2;
  }
}

class Simulation {
  List<Particule> particules;
  double dt;

  Simulation(this.particules, this.dt);

  void update() {
    for (Particule p in particules) {
      p.update(dt);

      // Vérifier les collisions entre les particules
      for (Particule q in particules) {
        if (p != q) {
          double dx = q.x - p.x;
          double dy = q.y - p.y;
          if (dx * dx + dy * dy < (p.r + q.r) * (p.r + q.r)) {
            p.collide(q);
          }
        }
      }
    }
  }
}

void main() {
  List<Particule> particules = [];
  for (int i = 0; i < 100; i++) {
    particules.add(Particule(
        Random().nextDouble(),
        Random().nextDouble(),
        Random().nextDouble() * 0.05,
        Random().nextDouble() * 0.05,
        0.001 + 0.001 * Random().nextDouble(),
        0.01));
  }

  Simulation simulation = Simulation(particules, 0.001);

  for (int i = 0; i < 10000; i++) {
    simulation.update();
  }

  // Affichage des résultats
  for (Particule p in particules) {
    print('Position : (${p.x}, ${p.y})');
    print('Vitesse : (${p.vx}, ${p.vy})');
    print('----------------------');
  }
}
```

**Explication du code :**

* **Classe Particule** : Représente une particule avec sa position, sa vitesse, sa masse et son rayon. Elle contient des méthodes pour mettre à jour sa position et détecter les collisions avec les bords et les autres particules.
* **Classe Simulation** : Gère la simulation des particules, met à jour leurs positions et gère les collisions entre elles.
* **Fonction main** : Initialise une liste de particules, crée une instance de Simulation et simule le mouvement des particules pendant un certain nombre d'étapes.
* **Méthode dot** : Calcule le produit scalaire entre deux vecteurs.
* **Méthode collide** : Gère la collision entre deux particules, en calculant les nouvelles vitesses après la collision en utilisant des équations de collision élastique.

Ce code simule le mouvement de 100 particules dans un espace bidimensionnel pendant 10 000 étapes de temps. Les particules se déplacent dans l'espace, rebondissent sur les bords et entrent en collision entre elles de manière élastique.