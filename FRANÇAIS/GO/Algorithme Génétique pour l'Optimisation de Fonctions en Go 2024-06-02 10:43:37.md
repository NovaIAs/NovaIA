**Algorithme génétique pour optimiser une fonction**

Voici un exemple de code complexe en Go qui implémente un algorithme génétique pour optimiser une fonction. L'algorithme génétique est un algorithme d'optimisation basé sur les principes de l'évolution naturelle.

```go
package main

import (
    "fmt"
    "math"
    "math/rand"
    "sort"
)

// Définir la fonction à optimiser
func f(x float64) float64 {
    return math.Sin(x) + math.Cos(x)
}

// Générer une population aléatoire
func generatePopulation(size int) []float64 {
    population := make([]float64, size)
    for i := range population {
        population[i] = rand.Float64()
    }
    return population
}

// Calculer le fitness de chaque individu
func calculateFitness(population []float64) []float64 {
    fitness := make([]float64, len(population))
    for i, x := range population {
        fitness[i] = f(x)
    }
    return fitness
}

// Sélectionner les individus les plus aptes
func selectParents(population, fitness []float64, numParents int) []float64 {
    // Trier la population par fitness
    sortedPopulation := sort.Float64Slice(population)
    sortedPopulation.Sort()

    // Sélectionner les individus les plus aptes
    parents := make([]float64, numParents)
    for i := 0; i < numParents; i++ {
        parents[i] = sortedPopulation[i]
    }
    return parents
}

// Croiser les parents pour créer des enfants
func crossover(parents []float64) []float64 {
    children := make([]float64, len(parents))
    for i := 0; i < len(children); i += 2 {
        // Choisir un point de croisement aléatoire
        crossoverPoint := rand.Intn(len(parents[0]))

        // Créer un enfant à partir des parents
        children[i] = parents[i%len(parents)][:crossoverPoint] + parents[(i+1)%len(parents)][crossoverPoint:]
        children[i+1] = parents[(i+1)%len(parents)][:crossoverPoint] + parents[i%len(parents)][crossoverPoint:]
    }
    return children
}

// Muter les enfants
func mutate(children []float64, mutationRate float64) []float64 {
    for i := range children {
        // Muter chaque enfant avec une probabilité `mutationRate`
        if rand.Float64() < mutationRate {
            // Choisir une mutation aléatoire
            mutationPoint := rand.Intn(len(children[0]))
            children[i][mutationPoint] = rand.Float64()
        }
    }
    return children
}

// Remplacer la population actuelle par la nouvelle génération
func replacePopulation(population, children []float64) {
    for i := range population {
        population[i] = children[i]
    }
}

func main() {
    // Paramètres de l'algorithme génétique
    populationSize := 100
    numParents := 20
    mutationRate := 0.1

    // Générer une population initiale
    population := generatePopulation(populationSize)

    // Boucler jusqu'à ce que l'optimum soit trouvé
    for i := 0; i < 1000; i++ {
        // Calculer le fitness de la population
        fitness := calculateFitness(population)

        // Sélectionner les individus les plus aptes
        parents := selectParents(population, fitness, numParents)

        // Croiser les parents pour créer des enfants
        children := crossover(parents)

        // Muter les enfants
        children = mutate(children, mutationRate)

        // Remplacer la population actuelle par la nouvelle génération
        replacePopulation(population, children)
    }

    // Obtenir le meilleur individu de la population
    bestIndividual := population[0]
    for _, x := range population {
        if f(x) > f(bestIndividual) {
            bestIndividual = x
        }
    }

    // Afficher le meilleur individu
    fmt.Println("La meilleure solution trouvée est :", bestIndividual)
}
```

**Explication du code**

* La fonction `f` définit la fonction à optimiser. Dans cet exemple, nous utilisons la fonction sinus + cosinus.
* La fonction `generatePopulation` génère une population aléatoire d'individus. Chaque individu est représenté par un flottant.
* La fonction `calculateFitness` calcule le fitness de chaque individu. Le fitness est une mesure de la qualité de l'individu, dans ce cas, il est égal à la valeur de la fonction à optimiser.
* La fonction `selectParents` sélectionne les individus les plus aptes de la population.
* La fonction `crossover` croise les parents pour créer des enfants. Le croisement est une opération qui combine les gènes (représentés par des flotteurs) des parents pour créer de nouveaux individus.
* La fonction `mutate` mute les enfants avec une certaine probabilité. La mutation est une opération qui modifie aléatoirement les gènes d'un individu.
* La fonction `replacePopulation` remplace la population actuelle par la nouvelle génération.
* La fonction `main` définit les paramètres de l'algorithme génétique et exécute l'algorithme.
* La fonction `main` trouve le meilleur individu de la population et l'affiche.