Claro! Aqui está um código complexo em Portugol que realiza uma busca em profundidade em um grafo representado por uma matriz de adjacência:

1. Início
2. Função BuscaProfundidade(matrizAdjacencia, verticeInicial)
3.     Variável visitados[numeroVertices] = {Falso}
4.     Pilha pilhaVertices
5.     visitados[verticeInicial] = Verdadeiro
6.     pilhaVertices.empilhar(verticeInicial)
7.     Enquanto pilhaVertices.naoVazia()
8.         Variável verticeAtual = pilhaVertices.desempilhar()
9.         Exibir "Visitando vértice: " + verticeAtual
10.        Para cada vizinho em matrizAdjacencia[verticeAtual]
11.            Se visitados[vizinho] = Falso
12.                visitados[vizinho] = Verdadeiro
13.                pilhaVertices.empilhar(vizinho)
14. Fim Função
15. 
16. Função Principal()
17.     Variável matriz = {{0, 1, 1, 0, 0},
18.                        {1, 0, 0, 1, 1},
19.                        {1, 0, 0, 0, 1},
20.                        {0, 1, 0, 0, 0},
21.                        {0, 1, 1, 0, 0}}
22.     Variável verticeInicial = 0
23.     BuscaProfundidade(matriz, verticeInicial)
24. Fim Função Principal
25. Fim

Explicação do código:
- Nas linhas 2 a 14, temos a função BuscaProfundidade, que recebe a matriz de adjacência e o vértice inicial como parâmetros. Ela inicia criando um vetor de visitados e uma pilha de vértices. O vetor de visitados é inicializado como falso para todos os vértices e o vértice inicial é marcado como visitado e empilhado na pilha. Em seguida, enquanto a pilha não estiver vazia, o código desempilha um vértice, exibe que está visitando esse vértice e percorre todos os seus vizinhos. Se um vizinho ainda não foi visitado, ele é marcado como visitado e empilhado na pilha.
- Na função principal, nas linhas 16 a 24, é criada a matriz de adjacência que representa o grafo. O vértice inicial é definido como 0 e a função BuscaProfundidade é chamada passando a matriz e o vértice inicial como argumentos.

Esse código realiza uma busca em profundidade em um grafo, visitando todos os vértices conectados a partir de um vértice inicial. Ele utiliza uma pilha para armazenar os vértices a serem visitados e um vetor de visitados para controlar quais vértices já foram visitados. Cada vez que um vértice é visitado, ele é marcado como visitado e seus vizinhos não visitados são empilhados para serem visitados posteriormente.