Claro! Aqui está um código em Go que implementa um algoritmo de busca em largura (BFS) para encontrar o menor caminho em um labirinto.

```go
package main

import (
	"fmt"
	"os"
	"bufio"
)

type Ponto struct {
	x, y int
}

var (
	entrada Ponto
	saida   Ponto
	infinito = 1 << 63 - 1
	dx       = []int{1, -1, 0, 0}
	dy       = []int{0, 0, 1, -1}
)

func main() {
	// Abre o arquivo de entrada
	arquivo, err := os.Open("labirinto.txt")
	if err != nil {
		fmt.Println("Erro ao abrir o arquivo:", err)
		return
	}
	defer arquivo.Close()

	// Lê as dimensões do labirinto
	var linhas, colunas int
	fmt.Fscanf(arquivo, "%d %d", &linhas, &colunas)

	// Lê o labirinto
	labirinto := make([][]rune, linhas)
	for i := 0; i < linhas; i++ {
		labirinto[i] = make([]rune, colunas)
		for j := 0; j < colunas; j++ {
			fmt.Fscanf(arquivo, "%c", &labirinto[i][j])
			if labirinto[i][j] == 'E' {
				entrada = Ponto{i, j}
			} else if labirinto[i][j] == 'S' {
				saida = Ponto{i, j}
			}
		}
		fmt.Fscanf(arquivo, "\n")
	}

	// Chama a função BFS para encontrar o menor caminho
	caminho, distancia := bfs(labirinto, entrada, saida)

	// Imprime o resultado
	if caminho == nil {
		fmt.Println("Não há caminho para a saída!")
	} else {
		fmt.Println("Menor caminho encontrado:")
		for _, ponto := range caminho {
			fmt.Printf("(%d, %d) ", ponto.x, ponto.y)
		}
		fmt.Println("\nDistância:", distancia)
	}
}

func bfs(labirinto [][]rune, entrada, saida Ponto) ([]Ponto, int) {
	// Inicializa a fila
	fila := make([]Ponto, 0)
	fila = append(fila, entrada)

	// Cria um mapa para armazenar as distâncias mínimas
	distancias := make(map[Ponto]int)
	for i := 0; i < len(labirinto); i++ {
		for j := 0; j < len(labirinto[i]); j++ {
			distancias[Ponto{i, j}] = infinito
		}
	}

	// Inicializa a distância da entrada como 0
	distancias[entrada] = 0

	// Cria um mapa para armazenar os pais de cada ponto
	pais := make(map[Ponto]Ponto)

	// Executa o algoritmo de busca em largura
	for len(fila) > 0 {
		p := fila[0]
		fila = fila[1:]

		if p == saida {
			break
		}

		for i := 0; i < 4; i++ {
			np := Ponto{p.x + dx[i], p.y + dy[i]}
			if np.x >= 0 && np.x < len(labirinto) && np.y >= 0 && np.y < len(labirinto[0]) && labirinto[np.x][np.y] != '#' && distancias[np] == infinito {
				fila = append(fila, np)
				distancias[np] = distancias[p] + 1
				pais[np] = p
			}
		}
	}

	// Constrói o caminho a partir dos pais
	if distancias[saida] != infinito {
		caminho := make([]Ponto, 0)
		p := saida
		for p != entrada {
			caminho = append(caminho, p)
			p = pais[p]
		}
		// Inverte a ordem do caminho
		for i, j := 0, len(caminho)-1; i < j; i, j = i+1, j-1 {
			caminho[i], caminho[j] = caminho[j], caminho[i]
		}
		return caminho, distancias[saida]
	}

	return nil, -1
}
```

Neste código, utilizamos uma estrutura de dados `Ponto` para representar as coordenadas de um ponto no labirinto. A função `main` lê o labirinto de um arquivo chamado "labirinto.txt" e chama a função `bfs` para encontrar o menor caminho da entrada para a saída.

A função `bfs` implementa o algoritmo de busca em largura (BFS). Inicialmente, ela cria uma fila para armazenar os pontos a serem visitados. Em seguida, inicializa um mapa `distancias` com distâncias iniciais infinitas para cada ponto do labirinto. O ponto de entrada é configurado com uma distância de 0.

O algoritmo, então, itera sobre a fila, removendo o primeiro ponto, e verifica se ele é o ponto de saída. Se for, o algoritmo para e retorna o caminho e a distância. Caso contrário, o algoritmo verifica os pontos vizinhos do ponto atual e os adiciona à fila se ainda não foram visitados.

Após a execução do algoritmo, a função `bfs` constrói o caminho a partir dos pais de cada ponto, armazenados no mapa `pais`. O caminho é retornado juntamente com a distância encontrada.

Espero que este código atenda às suas expectativas!