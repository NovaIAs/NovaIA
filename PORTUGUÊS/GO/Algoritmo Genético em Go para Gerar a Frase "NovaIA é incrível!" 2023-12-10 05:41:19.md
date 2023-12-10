Claro! Aqui está um código em Go que implementa um algoritmo de otimização conhecido como Algoritmo Genético. O algoritmo genético é inspirado no processo de seleção natural e é usado para resolver problemas de otimização.

```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

const (
	populationSize    = 100
	numGenerations    = 1000
	tournamentSize    = 5
	crossoverRate     = 0.7
	mutationRate      = 0.01
	targetString      = "NovaIA é incrível!"
	validCharacters   = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz! "
	elitismPercentage = 0.1
)

type Individual struct {
	Genes      []byte
	Fitness    float64
}

func main() {
	rand.Seed(time.Now().UnixNano())

	population := generateInitialPopulation()
	evaluatePopulation(&population)

	for generation := 1; generation <= numGenerations; generation++ {
		fmt.Printf("Generation %d: Best Fitness %.2f\n", generation, population[0].Fitness)

		nextGeneration := make([]Individual, populationSize)

		elitismOffset := int(populationSize * elitismPercentage)
		elitism(nextGeneration[:elitismOffset], population[:elitismOffset])

		for i := elitismOffset; i < populationSize; i++ {
			parent1 := tournamentSelection(population)
			parent2 := tournamentSelection(population)
			child := crossover(parent1, parent2)
			mutate(child)
			nextGeneration[i] = child
		}

		evaluatePopulation(&nextGeneration)

		population = nextGeneration
	}

	fmt.Printf("Best Individual: %s\n", string(population[0].Genes))
}

// Generate an initial population of random individuals
func generateInitialPopulation() []Individual {
	population := make([]Individual, populationSize)

	for i := 0; i < populationSize; i++ {
		genes := make([]byte, len(targetString))
		for j := 0; j < len(targetString); j++ {
			genes[j] = randomValidCharacter()
		}
		population[i] = Individual{Genes: genes}
	}

	return population
}

// Evaluate the fitness of each individual in the population
func evaluatePopulation(population *[]Individual) {
	for i := 0; i < populationSize; i++ {
		fitness := 0.0
		for j := 0; j < len(targetString); j++ {
			if (*population)[i].Genes[j] == targetString[j] {
				fitness++
			}
		}
		(*population)[i].Fitness = fitness / float64(len(targetString))
	}

	// Sort population by fitness in descending order
	sortPopulation(population)
}

// Perform elitism by selecting the best individuals
func elitism(nextGeneration []Individual, elite []Individual) {
	copy(nextGeneration, elite)
}

// Tournament selection to choose parent for crossover
func tournamentSelection(population []Individual) Individual {
	var bestIndividual Individual

	for i := 0; i < tournamentSize; i++ {
		index := rand.Intn(populationSize)
		individual := population[index]
		if individual.Fitness > bestIndividual.Fitness {
			bestIndividual = individual
		}
	}

	return bestIndividual
}

// Crossover two parents to create a child
func crossover(parent1 Individual, parent2 Individual) Individual {
	childGenes := make([]byte, len(targetString))

	for i := 0; i < len(targetString); i++ {
		if rand.Float64() < crossoverRate {
			childGenes[i] = parent1.Genes[i]
		} else {
			childGenes[i] = parent2.Genes[i]
		}
	}

	return Individual{Genes: childGenes}
}

// Mutate an individual by randomly changing a gene
func mutate(individual Individual) {
	for i := 0; i < len(targetString); i++ {
		if rand.Float64() < mutationRate {
			individual.Genes[i] = randomValidCharacter()
		}
	}
}

// Return a random valid character
func randomValidCharacter() byte {
	return validCharacters[rand.Intn(len(validCharacters))]
}

// Sort population by fitness in descending order
func sortPopulation(population *[]Individual) {
	for i := 0; i < populationSize-1; i++ {
		for j := i + 1; j < populationSize; j++ {
			if (*population)[i].Fitness < (*population)[j].Fitness {
				(*population)[i], (*population)[j] = (*population)[j], (*population)[i]
			}
		}
	}
}
```

Neste código, estamos tentando gerar a frase "NovaIA é incrível!" usando um algoritmo genético. O código utiliza uma estrutura chamada `Individual` para representar cada indivíduo na população. Cada indivíduo possui um conjunto de genes, que são sequências de caracteres aleatórios.

A função `generateInitialPopulation` gera uma população inicial com indivíduos aleatórios. Em seguida, a função `evaluatePopulation` calcula o fitness de cada indivíduo, que é a medida de quão próximo ele está da frase desejada. Os indivíduos são então ordenados em ordem decrescente de fitness.

O loop principal do algoritmo genético começa na função `main`. Ele itera por um número fixo de gerações. Em cada geração, uma nova população é gerada a partir da população atual. A elitism é aplicada, onde os melhores indivíduos da geração atual são mantidos na próxima geração. Em seguida, os pais são selecionados utilizando o método de seleção por torneio, e um filho é gerado utilizando o crossover. O filho é então mutado com uma pequena chance. Esse processo de seleção, crossover e mutação é repetido até que a nova população esteja completa. Finalmente, a nova população é avaliada e o loop continua com a próxima geração.

O código é executado por um número fixo de gerações e exibe o melhor indivíduo encontrado. O objetivo é que, ao longo das gerações, o algoritmo genético seja capaz de gerar indivíduos cada vez mais próximos da frase desejada.