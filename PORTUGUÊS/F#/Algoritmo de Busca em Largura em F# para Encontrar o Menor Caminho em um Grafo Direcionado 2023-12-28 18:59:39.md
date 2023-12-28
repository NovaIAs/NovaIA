Claro! Aqui está um exemplo de código complexo em F# que implementa um algoritmo de busca em largura para encontrar o menor caminho em um grafo direcionado:

```fsharp
open System.Collections.Generic

type Grafo<'T> = 
    { Vertices : 'T list
      Arestas : ('T * 'T) list }

let buscaEmLargura<'T> (grafo: Grafo<'T>) (inicio: 'T) (fim: 'T) =
    let fila = new Queue<'T>()
    let visitados = new HashSet<'T>()
    let predecessores = new Dictionary<'T, 'T>()

    fila.Enqueue inicio
    visitados.Add inicio

    while fila.Count > 0 do
        let verticeAtual = fila.Dequeue()

        if verticeAtual = fim then
            let caminho = ref [verticeAtual]
            let mutable pred = predecessores.[verticeAtual]
            while pred <> inicio do
                caminho := pred :: !caminho
                pred <- predecessores.[pred]

            caminho := inicio :: !caminho
            printfn "Menor caminho encontrado: %A" !caminho
            printfn ""

        for (verticeOrigem, verticeDestino) in grafo.Arestas do
            if verticeOrigem = verticeAtual && not (visitados.Contains verticeDestino) then
                fila.Enqueue verticeDestino
                visitados.Add verticeDestino
                predecessores.[verticeDestino] <- verticeAtual

let grafoExemplo =
    { Vertices = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
      Arestas = [(1, 2); (1, 3); (2, 4); (2, 5); (3, 6); (3, 7); (4, 8); (5, 8); (6, 9); (7, 9); (8, 10); (9, 10)] }

buscaEmLargura grafoExemplo 1 10
```

Neste código, começamos definindo um tipo `Grafo` que contém uma lista de vértices e uma lista de arestas. Em seguida, temos a função `buscaEmLargura`, que recebe o grafo, o vértice de início e o vértice de fim como parâmetros.

Dentro da função `buscaEmLargura`, criamos uma fila para armazenar os vértices a serem visitados, um conjunto para registrar os vértices já visitados e um dicionário para guardar os predecessores de cada vértice. Iniciamos a busca adicionando o vértice de início na fila e marcando-o como visitado.

Em um loop, continuamos a busca enquanto houver vértices na fila. Para cada vértice atual da fila, verificamos se é o vértice de fim desejado. Se for, rastreamos o menor caminho percorrendo os predecessores a partir do vértice de fim até o vértice de início e imprimimos o caminho encontrado.

Em seguida, percorremos todas as arestas do grafo e adicionamos na fila os vértices de destino das arestas que partem do vértice atual, desde que esses vértices ainda não tenham sido visitados. Além disso, registramos o vértice atual como predecessor desses vértices.

No final do código, criamos um exemplo de grafo chamado `grafoExemplo` e chamamos a função `buscaEmLargura` passando o grafo, o vértice de início 1 e o vértice de fim 10 como argumentos.

Este código utiliza a estrutura de dados Queue da biblioteca System.Collections.Generic para implementar a fila, HashSet para registrar os vértices visitados e Dictionary para armazenar os predecessores de cada vértice.