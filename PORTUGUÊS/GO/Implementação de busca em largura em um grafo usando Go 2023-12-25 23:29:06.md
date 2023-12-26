Claro! Aqui está um código complexo em Go que envolve a implementação de um algoritmo de busca em largura em um grafo. Este código utiliza uma estrutura de dados especializada para representar o grafo e uma fila para a ordem de visita dos nós.

```go
package main

import (
	"fmt"
)

type Grafo struct {
	vertices  []Vertice
}

type Vertice struct {
	valor   int
	vizinhos []int
}

func (g *Grafo) adicionarVertice(valor int) {
	v := Vertice{
		valor:   valor,
		vizinhos: []int{},
	}
	g.vertices = append(g.vertices, v)
}

func (g *Grafo) adicionarAresta(origem, destino int) {
	g.vertices[origem].vizinhos = append(g.vertices[origem].vizinhos, destino)
	g.vertices[destino].vizinhos = append(g.vertices[destino].vizinhos, origem)
}

func buscaEmLargura(g Grafo, inicio int) {
	visitados := make([]bool, len(g.vertices))
	fila := []int{inicio}

	for len(fila) > 0 {
		vertice := fila[0]
		fila = fila[1:]
		visitados[vertice] = true

		fmt.Printf("Visitando vértice %d\n", vertice)

		for _, vizinho := range g.vertices[vertice].vizinhos {
			if !visitados[vizinho] {
				fila = append(fila, vizinho)
				visitados[vizinho] = true
			}
		}
	}
}

func main() {
	g := Grafo{}

	// Adicionando vértices
	for i := 0; i < 7; i++ {
		g.adicionarVertice(i)
	}

	// Adicionando arestas
	g.adicionarAresta(0, 1)
	g.adicionarAresta(0, 2)
	g.adicionarAresta(1, 3)
	g.adicionarAresta(1, 4)
	g.adicionarAresta(2, 5)
	g.adicionarAresta(2, 6)

	fmt.Println("Busca em largura:")
	buscaEmLargura(g, 0)
}
```

Neste código, começamos definindo a estrutura de dados `Grafo`, que consiste em um array de `Vertice`. Cada `Vertice` possui um valor e uma lista de vizinhos, representada por um array de inteiros.

Em seguida, temos os métodos `adicionarVertice` e `adicionarAresta`, que permitem adicionar vértices e arestas ao grafo, respectivamente.

A função `buscaEmLargura` implementa o algoritmo de busca em largura no grafo. Ela recebe o grafo e um vértice de início como parâmetros. A função utiliza uma fila para armazenar os próximos vértices a serem visitados e um array de booleanos para controlar quais vértices já foram visitados. O algoritmo itera sobre a fila, visitando cada vértice e adicionando os vizinhos não visitados à fila.

No `main`, criamos um novo grafo, adicionamos vértices e arestas e chamamos a função `buscaEmLargura` para realizar a busca a partir do vértice 0.

Espero que este código complexo atenda às suas expectativas!