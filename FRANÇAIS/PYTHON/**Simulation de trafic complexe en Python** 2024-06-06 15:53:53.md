**Programme de simulation de trafic complexe en python**

**Objectif :**
Simuler un système de trafic urbain avec des véhicules, des intersections, des feux de circulation et des capteurs.

**Code :**

```python
import random
import numpy as np
import matplotlib.pyplot as plt

class Vehicule:
    def __init__(self, id, type_vehicule, vitesse_initiale):
        self.id = id
        self.type_vehicule = type_vehicule
        self.vitesse = vitesse_initiale
        self.position = [0, 0]  # position initiale
        self.destination = [100, 100]  # destination par défaut

    def deplacer(self, dt):
        deplacement = self.vitesse * dt
        self.position[0] += deplacement[0]
        self.position[1] += deplacement[1]

class Intersection:
    def __init__(self, id, position, feu_circulation):
        self.id = id
        self.position = position
        self.feu_circulation = feu_circulation

class Capteur:
    def __init__(self, id, position, rayon_detection):
        self.id = id
        self.position = position
        self.rayon_detection = rayon_detection
        self.vehicules_detectes = []

    def detecter_vehicules(self, vehicules):
        self.vehicules_detectes = []
        for vehicule in vehicules:
            distance = np.linalg.norm(np.array(vehicule.position) - np.array(self.position))
            if distance <= self.rayon_detection:
                self.vehicules_detectes.append(vehicule)

class SystemeTrafic:
    def __init__(self, nb_vehicules, nb_intersections, nb_capteurs):
        self.vehicules = []
        self.intersections = []
        self.capteurs = []

        for i in range(nb_vehicules):
            self.vehicules.append(Vehicule(i, random.choice(['voiture', 'camion', 'bus']),
                                           np.random.uniform(10, 20)))

        for i in range(nb_intersections):
            self.intersections.append(Intersection(i, [random.randint(0, 100), random.randint(0, 100)],
                                                   FeuCirculation()))

        for i in range(nb_capteurs):
            self.capteurs.append(Capteur(i, [random.randint(0, 100), random.randint(0, 100)], 20))

    def simuler(self, dt, nb_iterations):
        for iteration in range(nb_iterations):
            for vehicule in self.vehicules:
                vehicule.deplacer(dt)
                for capteur in self.capteurs:
                    capteur.detecter_vehicules(self.vehicules)

class FeuCirculation:
    def __init__(self):
        self.etat = 'rouge'
        self.temps_reste = 5

    def mettre_a_jour(self, dt):
        self.temps_reste -= dt
        if self.temps_reste <= 0:
            if self.etat == 'rouge':
                self.etat = 'vert'
                self.temps_reste = 5
            elif self.etat == 'vert':
                self.etat = 'orange'
                self.temps_reste = 2
            elif self.etat == 'orange':
                self.etat = 'rouge'
                self.temps_reste = 5
```

**Explication du code :**

* La classe `Vehicule` représente un véhicule avec son identifiant, son type, sa vitesse, sa position et sa destination.
* La classe `Intersection` représente une intersection avec son identifiant, sa position et son feu de circulation.
* La classe `Capteur` représente un capteur avec son identifiant, sa position et son rayon de détection.
* La classe `SystemeTrafic` représente le système de trafic dans son ensemble, comprenant les véhicules, les intersections et les capteurs.
* La classe `FeuCirculation` représente un feu de circulation avec son état (rouge, vert, orange) et son temps restant.

La fonction `simuler` simule le système de trafic pendant un certain nombre d'itérations et un pas de temps donné. À chaque itération, les véhicules se déplacent, les capteurs détectent les véhicules et les feux de circulation se mettent à jour.