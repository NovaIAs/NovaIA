```typescript
// Este código implementa un algoritmo genético para encontrar la solución óptima a un problema.

// Definimos la interfaz de un gen, que es una representación de una solución posible.
interface Gen {
  // El valor del gen.
  value: number;
  // La aptitud del gen, que es una medida de cuán bueno es.
  fitness: number;
}

// Definimos la interfaz de una población, que es un conjunto de genes.
interface Population {
  // Los genes de la población.
  genes: Gen[];
  // La aptitud media de la población.
  averageFitness: number;
  // El mejor gen de la población.
  bestGene: Gen;
}

// Definimos la interfaz de un algoritmo genético.
interface GeneticAlgorithm {
  // La población inicial.
  initialPopulation: Population;
  // El número de generaciones a ejecutar.
  generations: number;
  // La tasa de mutación.
  mutationRate: number;
  // La tasa de cruce.
  crossoverRate: number;

  // Ejecuta el algoritmo genético y devuelve la mejor solución encontrada.
  run(): Gen;
}

// Implementamos el algoritmo genético.
class GeneticAlgorithm implements GeneticAlgorithm {
  // La población inicial.
  initialPopulation: Population;
  // El número de generaciones a ejecutar.
  generations: number;
  // La tasa de mutación.
  mutationRate: number;
  // La tasa de cruce.
  crossoverRate: number;

  constructor(initialPopulation: Population, generations: number, mutationRate: number, crossoverRate: number) {
    this.initialPopulation = initialPopulation;
    this.generations = generations;
    this.mutationRate = mutationRate;
    this.crossoverRate = crossoverRate;
  }

  // Ejecuta el algoritmo genético y devuelve la mejor solución encontrada.
  run(): Gen {
    // Inicializamos la población actual con la población inicial.
    let currentPopulation = this.initialPopulation;

    // Iteramos sobre las generaciones.
    for (let i = 0; i < this.generations; i++) {
      // Evaluamos la aptitud de cada gen en la población actual.
      currentPopulation = this.evaluatePopulation(currentPopulation);

      // Seleccionamos los genes más aptos para la reproducción.
      currentPopulation = this.selectPopulation(currentPopulation);

      // Cruzamos los genes seleccionados para crear nuevos genes.
      currentPopulation = this.crossoverPopulation(currentPopulation);

      // Mutamos los nuevos genes.
      currentPopulation = this.mutatePopulation(currentPopulation);
    }

    // Devolvemos el mejor gen de la población final.
    return currentPopulation.bestGene;
  }

  // Evalúa la aptitud de cada gen en la población.
  evaluatePopulation(population: Population): Population {
    // Iteramos sobre los genes de la población.
    for (let i = 0; i < population.genes.length; i++) {
      // Calculamos la aptitud del gen.
      population.genes[i].fitness = this.calculateFitness(population.genes[i]);
    }

    // Calculamos la aptitud media de la población.
    population.averageFitness = this.calculateAverageFitness(population);

    // Devolvemos la población con los genes evaluados.
    return population;
  }

  // Selecciona los genes más aptos para la reproducción.
  selectPopulation(population: Population): Population {
    // Creamos una nueva población vacía.
    let newPopulation: Population = {
      genes: [],
      averageFitness: 0,
      bestGene: null
    };

    // Iteramos sobre los genes de la población.
    while (newPopulation.genes.length < population.genes.length) {
      // Seleccionamos dos genes aleatorios de la población.
      let parent1 = this.selectGene(population);
      let parent2 = this.selectGene(population);

      // Cruzamos los dos genes seleccionados para crear un nuevo gen.
      let child = this.crossoverGenes(parent1, parent2);

      // Mutamos el nuevo gen.
      child = this.mutateGene(child);

      // Añadimos el nuevo gen a la nueva población.
      newPopulation.genes.push(child);
    }

    // Calculamos la aptitud media de la nueva población.
    newPopulation.averageFitness = this.calculateAverageFitness(newPopulation);

    // Devolvemos la nueva población.
    return newPopulation;
  }

  // Cruza dos genes para crear un nuevo gen.
  crossoverGenes(parent1: Gen, parent2: Gen): Gen {
    // Generamos un número aleatorio entre 0 y 1.
    let randomNumber = Math.random();

    // Si el número aleatorio es menor que la tasa de cruce, cruzamos los dos genes.
    if (randomNumber < this.crossoverRate) {
      // Generamos un punto de cruce aleatorio.
      let crossoverPoint = Math.floor(Math.random() * parent1.value.length);

      // Creamos un nuevo gen con la parte izquierda del primer gen y la parte derecha del segundo gen.
      let child = {
        value: parent1.value.substring(0, crossoverPoint) + parent2.value.substring(crossoverPoint),
        fitness: 0
      };

      // Devolvemos el nuevo gen.
      return child;
    }
    // Si el número aleatorio es mayor o igual que la tasa