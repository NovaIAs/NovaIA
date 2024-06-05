**Fonctionnalité :** Implémentation d'un algorithme génétique pour optimiser une fonction cible

```python
import random
import numpy as np

def fitness(individu, cible):
    """Calcul de la fitness d'un individu.

    Args:
        individu (list): Liste de valeurs représentant l'individu.
        cible (float): Valeur cible à atteindre.

    Returns:
        float: Fitness de l'individu.
    """
    return np.abs(cible - np.mean(individu))

def selection(population, fitness_values):
    """Sélection des individus les plus aptes pour la reproduction.

    Args:
        population (list): Liste d'individus.
        fitness_values (list): Liste des valeurs de fitness associées à chaque individu.

    Returns:
        list: Liste des individus sélectionnés.
    """
    fitness_cumul = np.cumsum(fitness_values)
    selection_probabilities = fitness_cumul / fitness_cumul[-1]
    return [random.choices(population, weights=selection_probabilities)[0] for _ in range(len(population))]

def mutation(individu, taux_mutation):
    """Mutation d'un individu.

    Args:
        individu (list): Liste de valeurs représentant l'individu.
        taux_mutation (float): Taux de mutation.

    Returns:
        list: Individu muté.
    """
    for i in range(len(individu)):
        if random.random() < taux_mutation:
            individu[i] = random.random()
    return individu

def croisement(parents, taux_croisement):
    """Croisement de deux parents pour créer un nouvel individu.

    Args:
        parents (list): Liste des deux parents.
        taux_croisement (float): Taux de croisement.

    Returns:
        list: Individu croisé.
    """
    if random.random() < taux_croisement:
        point_croisement = random.randint(0, len(parents[0]) - 1)
        return parents[0][:point_croisement] + parents[1][point_croisement:]
    return parents[0]

def algorithme_genetique(cible, taille_population, generations, taux_mutation, taux_croisement):
    """Algorithme génétique pour optimiser une fonction cible.

    Args:
        cible (float): Valeur cible à atteindre.
        taille_population (int): Taille de la population.
        generations (int): Nombre de générations.
        taux_mutation (float): Taux de mutation.
        taux_croisement (float): Taux de croisement.

    Returns:
        list: Liste des meilleurs individus de chaque génération.
    """
    # Initialisation de la population aléatoire
    population = [np.random.rand(10) for _ in range(taille_population)]

    # Calcul des valeurs de fitness initiales
    fitness_values = [fitness(individu, cible) for individu in population]

    # Boucle principale de l'algorithme génétique
    meilleurs_individus = []
    for generation in range(generations):
        # Sélection des individus les plus aptes
        parents = selection(population, fitness_values)

        # Création de nouveaux individus par croisement et mutation
        nouveaux_individus = []
        for parent1, parent2 in zip(parents, parents[1:]):
            nouveaux_individus.append(croisement(parent1, parent2, taux_croisement))

        population = nouveaux_individus

        # Mutation des nouveaux individus
        for individu in population:
            individu = mutation(individu, taux_mutation)

        # Calcul des valeurs de fitness de la nouvelle population
        fitness_values = [fitness(individu, cible) for individu in population]

        # Enregistrement des meilleurs individus de la génération
        meilleurs_individus.append(min(population, key=lambda individu: fitness(individu, cible)))

    return meilleurs_individus
```