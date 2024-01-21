```portuguol
programa Grafo {
	tipo registro Vertice {
		char Nome;
		int Distancia;
	} vertice;

	tipo registro Grafo {
		int NumVertices;
		int NumArestas;
		Vertice Vertices[100];
		int MatrizAdjacencia[100][100];
	} grafo;

	função int InicializaGrafo(Grafo *g) {
		g->NumVertices = 0;
		g->NumArestas = 0;

		int i, j;
		for (i = 0; i < 100; i++) {
			for (j = 0; j < 100; j++) {
				g->MatrizAdjacencia[i][j] = 0;
			}
		}

		return 0;
	}

	função int AdicionaVertice(Grafo *g, char nome) {
		if (g->NumVertices == 100) {
			return -1;  // Grafo cheio
		}

		g->Vertices[g->NumVertices].Nome = nome;
		g->Vertices[g->NumVertices].Distancia = 0;
		g->NumVertices++;

		return 0;
	}

	função int AdicionaAresta(Grafo *g, int v1, int v2, int peso) {
		if (v1 < 0 || v1 >= g->NumVertices || v2 < 0 || v2 >= g->NumVertices) {
			return -1;  // Vértices inválidos
		}

		if (g->MatrizAdjacencia[v1][v2] != 0) {
			return -1;  // Aresta já existe
		}

		g->MatrizAdjacencia[v1][v2] = peso;
		g->MatrizAdjacencia[v2][v1] = peso;
		g->NumArestas++;

		return 0;
	}

	função int BuscaLargura(Grafo g, int inicio, int fim) {
		if (inicio < 0 || inicio >= g.NumVertices || fim < 0 || fim >= g.NumVertices) {
			return -1;  // Vértices inválidos
		}

		int visitados[100];
		int i, v;

		for (i = 0; i < g.NumVertices; i++) {
			visitados[i] = 0;
			g.Vertices[i].Distancia = -1;
		}

		visitados[inicio] = 1;
		g.Vertices[inicio].Distancia = 0;

		int fila[100];
		int frente = 0;
		int tras = 0;

		fila[tras++] = inicio;

		while (frente != tras) {
			v = fila[frente++];

			for (i = 0; i < g.NumVertices; i++) {
				if (g.MatrizAdjacencia[v][i] != 0 && !visitados[i]) {
					visitados[i] = 1;
					g.Vertices[i].Distancia = g.Vertices[v].Distancia + 1;
					fila[tras++] = i;
				}
			}
		}

		return g.Vertices[fim].Distancia;
	}

	função int main() {
		Grafo g;
		InicializaGrafo(&g);

		AdicionaVertice(&g, 'A');
		AdicionaVertice(&g, 'B');
		AdicionaVertice(&g, 'C');
		AdicionaVertice(&g, 'D');
		AdicionaVertice(&g, 'E');

		AdicionaAresta(&g, 0, 1, 5);
		AdicionaAresta(&g, 0, 2, 10);
		AdicionaAresta(&g, 1, 3, 15);
		AdicionaAresta(&g, 2, 3, 20);
		AdicionaAresta(&g, 2, 4, 25);
		AdicionaAresta(&g, 3, 4, 30);

		int distancia = BuscaLargura(g, 0, 4);
		if (distancia == -1) {
			printf("Não há caminho entre os vértices.");
		} else {
			printf("A distância entre os vértices A e E é %d.", distancia);
		}

		return 0;
	}
}
```

Explicação do código:

* O código implementa um grafo não direcionado com pesos nas arestas.
* O grafo é representado por uma estrutura de dados que contém o número de vértices, o número de arestas, um array de vértices e uma matriz de adjacência.
* Cada vértice é representado por uma estrutura de dados que contém o nome do vértice e sua distância do vértice inicial.
* A matriz de adjacência é uma matriz bidimensional que contém os pesos das arestas entre os vértices.
* O código contém funções para inicializar o grafo, adicionar vértices e arestas, e realizar uma busca em largura para encontrar o caminho mais curto entre dois vértices.
* A função `main()` cria um grafo, adiciona vértices e arestas, e realiza uma busca em largura para encontrar o caminho mais curto entre os vértices A e E.
* O código imprime a distância entre os vértices A e E, ou uma mensagem de erro se não houver caminho entre eles.