Claro! Aqui está um código complexo em Go que envolve a implementação de um algoritmo genético para resolver o problema do caixeiro viajante (TSP - Traveling Salesman Problem).

```go
package main

import (
	"fmt"
	"math"
	"math/rand"
)

type City struct {
	x int
	y int
}

type Route struct {
	cities []City
}

type Population struct {
	routes []Route
}

func calculateDistance(city1 City, city2 City) float64 {
	xDiff := float64(city2.x - city1.x)
	yDiff := float64(city2.y - city1.y)
	return math.Sqrt(math.Pow(xDiff, 2) + math.Pow(yDiff, 2))
}

func calculateTotalDistance(route Route) float64 {
	totalDistance := 0.0
	for i := 0; i < len(route.cities)-1; i++ {
		totalDistance += calculateDistance(route.cities[i], route.cities[i+1])
	}
	return totalDistance
}

func generateRandomRoute(cities []City) Route {
	route := Route{cities: make([]City, len(cities))}
	perm := rand.Perm(len(cities))
	for i, randIndex := range perm {
		route.cities[i] = cities[randIndex]
	}
	return route
}

func generateInitialPopulation(cities []City, populationSize int) Population {
	population := Population{routes: make([]Route, populationSize)}
	for i := 0; i < populationSize; i++ {
		population.routes[i] = generateRandomRoute(cities)
	}
	return population
}

func selectParents(population Population) (Route, Route) {
	parent1Index := rand.Intn(len(population.routes))
	parent2Index := rand.Intn(len(population.routes))
	parent1 := population.routes[parent1Index]
	parent2 := population.routes[parent2Index]
	return parent1, parent2
}

func crossover(parent1 Route, parent2 Route) Route {
	child := Route{cities: make([]City, len(parent1.cities))}
	child.cities[0] = parent1.cities[0]
	child.cities[len(parent1.cities)-1] = parent1.cities[len(parent1.cities)-1]
	for i := 1; i < len(parent1.cities)-1; i++ {
		if rand.Float64() < 0.5 {
			child.cities[i] = parent1.cities[i]
		} else {
			child.cities[i] = parent2.cities[i]
		}
	}
	return child
}

func mutate(route Route, mutationRate float64) Route {
	for i := 1; i < len(route.cities)-1; i++ {
		if rand.Float64() < mutationRate {
			j := rand.Intn(len(route.cities)-2) + 1
			route.cities[i], route.cities[j] = route.cities[j], route.cities[i]
		}
	}
	return route
}

func evolvePopulation(population Population, mutationRate float64) Population {
	newPopulation := Population{routes: make([]Route, len(population.routes))}
	newPopulation.routes[0] = getBestRoute(population)
	for i := 1; i < len(population.routes); i++ {
		parent1, parent2 := selectParents(population)
		child := crossover(parent1, parent2)
		child = mutate(child, mutationRate)
		newPopulation.routes[i] = child
	}
	return newPopulation
}

func getBestRoute(population Population) Route {
	bestRoute := population.routes[0]
	bestDistance := calculateTotalDistance(bestRoute)

	for _, route := range population.routes {
		distance := calculateTotalDistance(route)
		if distance < bestDistance {
			bestRoute = route
			bestDistance = distance
		}
	}
	return bestRoute
}

func main() {
	cities := []City{
		{x: 0, y: 0},
		{x: 1, y: 5},
		{x: 2, y: 10},
		{x: 3, y: 15},
		{x: 4, y: 20},
		{x: 5, y: 25},
	}

	populationSize := 100
	mutationRate := 0.01
	generations := 1000

	population := generateInitialPopulation(cities, populationSize)

	for i := 0; i < generations; i++ {
		population = evolvePopulation(population, mutationRate)
	}

	bestRoute := getBestRoute(population)
	bestDistance := calculateTotalDistance(bestRoute)

	fmt.Printf("Melhor caminho encontrado: ")
	for _, city := range bestRoute.cities {
		fmt.Printf("(%d,%d) ", city.x, city.y)
	}
	fmt.Printf("\nDistância total: %.2f\n", bestDistance)
}
```

Este código em Go implementa um algoritmo genético para resolver o problema do caixeiro viajante (TSP). O algoritmo genético é uma técnica de busca heurística inspirada na evolução biológica que utiliza conceitos como seleção natural, crossover e mutação para buscar soluções ótimas ou aproximadas para problemas de otimização.

Aqui estão as principais etapas do código:

1. Definir as estruturas de dados necessárias: `City`, `Route` e `Population`.
2. Implementar funções auxiliares para calcular a distância entre duas cidades (`calculateDistance`) e a distância total de uma rota (`calculateTotalDistance`).
3. Gerar uma rota aleatória (`generateRandomRoute`).
4. Gerar uma população inicial de rotas aleatórias (`generateInitialPopulation`).
5. Selecionar os pais para reprodução (`selectParents`).
6. Realizar o crossover entre os pais para gerar um filho (`crossover`).
7. Realizar a mutação em um filho (`mutate`).
8. Evoluir a população através de seleção, crossover e mutação (`evolvePopulation`).
9. Obter a melhor rota da população (`getBestRoute`).
10. Executar o algoritmo genético por um número determinado de gerações (`generations`).
11. Imprimir a melhor rota encontrada e sua distância total.

Este código é apenas um exemplo básico de implementação de um algoritmo genético para resolver o problema do caixeiro viajante em Go. É importante notar que o desempenho e a precisão da solução dependem de vários fatores, como a qualidade da função de fitness, o tamanho da população, a taxa de mutação e o número de gerações.