Claro! Abaixo está um código complexo em Swift que envolve a criação de uma estrutura de dados personalizada chamada "Graph" (Grafo) e implementa o algoritmo de busca em largura (Breadth-First Search - BFS) para encontrar o caminho mais curto entre dois vértices em um grafo não direcionado.

```swift
struct Graph {
    var nodes: [Int] // Lista de vértices do grafo
    var edges: [[Int]] // Lista de arestas do grafo
    var adjMatrix: [[Bool]] // Matriz de adjacência

    init(nodes: [Int], edges: [[Int]]) {
        self.nodes = nodes
        self.edges = edges
        self.adjMatrix = [[Bool]](repeating: [Bool](repeating: false, count: nodes.count), count: nodes.count)

        // Criando a matriz de adjacência
        for edge in edges {
            let u = edge[0]
            let v = edge[1]
            adjMatrix[u][v] = true
            adjMatrix[v][u] = true
        }
    }

    // Algoritmo de busca em largura (BFS)
    func bfs(source: Int, destination: Int) -> [Int] {
        var visited = [Bool](repeating: false, count: nodes.count) // Array para marcar os vértices visitados
        var parent = [Int](repeating: -1, count: nodes.count) // Array para armazenar o caminho percorrido

        var queue = [Int]() // Fila para armazenar os vértices a serem visitados
        queue.append(source)
        visited[source] = true

        while !queue.isEmpty {
            let current = queue.removeFirst()

            if current == destination {
                break // Encontrou o destino, sai do loop
            }

            for i in 0..<nodes.count {
                if adjMatrix[current][i] && !visited[i] {
                    queue.append(i)
                    visited[i] = true
                    parent[i] = current
                }
            }
        }

        // Construindo o caminho percorrido
        var path = [Int]()
        var current = destination
        while current != -1 {
            path.insert(current, at: 0)
            current = parent[current]
        }

        return path
    }
}

// Exemplo de uso
let nodes = [0, 1, 2, 3, 4, 5]
let edges = [[0, 1], [0, 2], [1, 3], [2, 4], [3, 4], [3, 5], [4, 5]]

let graph = Graph(nodes: nodes, edges: edges)
let source = 0
let destination = 5

let shortestPath = graph.bfs(source: source, destination: destination)
print("Caminho mais curto de \(source) para \(destination): \(shortestPath)")
```

Neste código, começamos definindo a estrutura de dados "Graph" que armazena os vértices e as arestas do grafo, bem como a matriz de adjacência. O construtor da estrutura de dados inicializa a matriz de adjacência com base nas arestas fornecidas.

Em seguida, temos a implementação do algoritmo BFS, que é responsável por encontrar o caminho mais curto entre dois vértices. O algoritmo usa uma fila para armazenar os vértices a serem visitados e um array para marcar os vértices que já foram visitados. Ele continua a visita até encontrar o destino ou até que não haja mais vértices na fila.

No final, temos um exemplo de uso onde criamos um grafo com alguns vértices e arestas e chamamos o algoritmo bfs para encontrar o caminho mais curto entre dois vértices específicos. O caminho mais curto é impresso no console.

Espero que este código atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais esclarecimentos, fique à vontade para perguntar.