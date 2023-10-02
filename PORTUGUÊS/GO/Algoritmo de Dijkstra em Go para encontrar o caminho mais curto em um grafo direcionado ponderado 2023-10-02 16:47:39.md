Claro! Aqui está um código complexo em Go que implementa o algoritmo de Dijkstra para encontrar o caminho mais curto em um grafo direcionado ponderado:

```go
package main

import (
	"fmt"
	"math"
)

type Grafo struct {
	vertices []Vertice
	arestas  [][]int
}

type Vertice struct {
	id         int
	distancia  int
	visitado   bool
	anterior   *Vertice
}

func criarGrafo(numVertices int) Grafo {
	grafo := Grafo{}
	grafo.vertices = make([]Vertice, numVertices)
	grafo.arestas = make([][]int, numVertices)

	for i := 0; i < numVertices; i++ {
		grafo.vertices[i].id = i
		grafo.vertices[i].distancia = math.MaxInt32
		grafo.vertices[i].visitado = false
		grafo.vertices[i].anterior = nil

		grafo.arestas[i] = make([]int, numVertices)
		for j := 0; j < numVertices; j++ {
			grafo.arestas[i][j] = math.MaxInt32
		}
	}

	return grafo
}

func adicionarAresta(grafo Grafo, origem int, destino int, peso int) {
	grafo.arestas[origem][destino] = peso
}

func minimoDistancia(grafo Grafo) *Vertice {
	minDistancia := math.MaxInt32
	var minVertice *Vertice

	for i := 0; i < len(grafo.vertices); i++ {
		if grafo.vertices[i].visitado == false && grafo.vertices[i].distancia <= minDistancia {
			minDistancia = grafo.vertices[i].distancia
			minVertice = &grafo.vertices[i]
		}
	}

	return minVertice
}

func dijkstra(grafo Grafo, origem int) {
	grafo.vertices[origem].distancia = 0

	for i := 0; i < len(grafo.vertices)-1; i++ {
		verticeAtual := minimoDistancia(grafo)
		verticeAtual.visitado = true

		for j := 0; j < len(grafo.vertices); j++ {
			if grafo.arestas[verticeAtual.id][j] != math.MaxInt32 && verticeAtual.visitado == false &&
				verticeAtual.distancia+grafo.arestas[verticeAtual.id][j] < grafo.vertices[j].distancia {
				grafo.vertices[j].distancia = verticeAtual.distancia + grafo.arestas[verticeAtual.id][j]
				grafo.vertices[j].anterior = verticeAtual
			}
		}
	}
}

func imprimirCaminho(grafo Grafo, destino int) {
	if grafo.vertices[destino].anterior != nil {
		imprimirCaminho(grafo, grafo.vertices[destino].anterior.id)
	}
	fmt.Printf("%d ", destino)
}

func main() {
	numVertices := 5
	grafo := criarGrafo(numVertices)

	adicionarAresta(grafo, 0, 1, 5)
	adicionarAresta(grafo, 0, 2, 10)
	adicionarAresta(grafo, 1, 3, 3)
	adicionarAresta(grafo, 2, 3, 1)
	adicionarAresta(grafo, 3, 4, 2)

	origem := 0
	destino := 4

	dijkstra(grafo, origem)

	fmt.Printf("Caminho mais curto do vértice %d para o vértice %d: ", origem, destino)
	imprimirCaminho(grafo, destino)
	fmt.Printf("\nDistância total: %d", grafo.vertices[destino].distancia)
}
```

Este código implementa o algoritmo de Dijkstra para encontrar o caminho mais curto em um grafo direcionado ponderado. Ele cria uma estrutura de dados `Grafo` que armazena os vértices e as arestas do grafo, e possui funções para adicionar arestas, encontrar o vértice com a menor distância, executar o algoritmo de Dijkstra e imprimir o caminho mais curto.

Neste exemplo, o grafo possui 5 vértices e as arestas têm pesos representados por números inteiros. O código encontra o caminho mais curto do vértice 0 para o vértice 4 e imprime o caminho e a distância total percorrida.