**Algorithme génétique pour l'optimisation de fonctions**

**Objectif:** Trouver le maximum d'une fonction donnée en utilisant un algorithme génétique.

**Principe:**

* Créer une population initiale de chromosomes (solutions potentielles).
* Évaluer l'aptitude de chaque chromosome (sa "qualité").
* Sélectionner les chromosomes les plus aptes pour la reproduction.
* Croiser et muter les chromosomes sélectionnés pour créer de nouveaux chromosomes.
* Réitérer les étapes ci-dessus jusqu'à ce qu'un critère d'arrêt soit atteint.

**Code:**

```javascript
const popSize = 100; // Taille de la population
const numGenes = 10; // Nombre de gènes par chromosome
const mutationRate = 0.1; // Taux de mutation
const crossoverRate = 0.7; // Taux de croisement

// Crée une population initiale de chromosomes
function initPopulation() {
    const population = [];
    for (let i = 0; i < popSize; i++) {
        const chromosome = [];
        for (let j = 0; j < numGenes; j++) {
            chromosome.push(Math.random());
        }
        population.push(chromosome);
    }
    return population;
}

// Évalue l'aptitude d'un chromosome
function evaluateFitness(chromosome) {
    // Fonction dont on veut optimiser le maximum ici
    const f = (x) => Math.sin(x);

    let sum = 0;
    for (let i = 0; i < numGenes; i++) {
        sum += chromosome[i] * f(chromosome[i]);
    }
    return sum;
}

// Sélectionne les chromosomes les plus aptes pour la reproduction
function selectParents(population, fitnesses) {
    const probabilities = [];
    for (let i = 0; i < popSize; i++) {
        probabilities.push(fitnesses[i] / sum(fitnesses));
    }

    const parents = [];
    for (let i = 0; i < popSize; i++) {
        const r = Math.random();
        let j = 0;
        while (j < popSize && probabilities[j] < r) {
            j++;
        }
        parents.push(population[j]);
    }
    return parents;
}

// Croise les chromosomes sélectionnés pour créer de nouveaux chromosomes
function crossover(parents) {
    const newPopulation = [];
    for (let i = 0; i < popSize; i++) {
        const p1 = parents[Math.floor(Math.random() * parents.length)];
        const p2 = parents[Math.floor(Math.random() * parents.length)];

        const newChromosome = [];
        for (let j = 0; j < numGenes; j++) {
            if (Math.random() < crossoverRate) {
                newChromosome.push(p1[j]);
            } else {
                newChromosome.push(p2[j]);
            }
        }
        newPopulation.push(newChromosome);
    }
    return newPopulation;
}

// Mute les chromosomes de la nouvelle population
function mutate(population) {
    for (let i = 0; i < popSize; i++) {
        for (let j = 0; j < numGenes; j++) {
            if (Math.random() < mutationRate) {
                population[i][j] += Math.random() * 0.1;
            }
        }
    }
    return population;
}

// Main
const population = initPopulation();
let generation = 0;
while (generation < 100) {
    const fitnesses = population.map(evaluateFitness);
    const parents = selectParents(population, fitnesses);
    const newPopulation = crossover(parents);
    const mutatedPopulation = mutate(newPopulation);
    population = mutatedPopulation;
    
    // Affichage du meilleur chromosome de la génération courante
    const bestChromosome = population[0];
    const bestFitness = evaluateFitness(bestChromosome);
    console.log(`Génération ${generation}: Meilleur chromosome = ${bestChromosome.join(', ')}, Aptitude = ${bestFitness.toFixed(2)}`);
    
    generation++;
}
```

**Explications:**

* **initPopulation:** Crée une population initiale de solutions aléatoires.
* **evaluateFitness:** Calcule l'aptitude d'un chromosome (ici, la somme des valeurs de la fonction pour chaque gène).
* **selectParents:** Sélectionne les chromosomes les plus aptes à partir de la population.
* **crossover:** Croise les chromosomes pour créer de nouveaux chromosomes, échangeant des gènes avec une certaine probabilité.
* **mutate:** Modifie aléatoirement les gènes des chromosomes avec une certaine probabilité.
* **Main:** Exécute l'algorithme génétique pendant un nombre spécifié de générations, affichant le meilleur chromosome et son aptitude à chaque génération.