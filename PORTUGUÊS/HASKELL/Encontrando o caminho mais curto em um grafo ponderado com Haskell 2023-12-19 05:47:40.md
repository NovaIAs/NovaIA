Claro! Aqui está um código em Haskell que utiliza o algoritmo de Dijkstra para encontrar o caminho mais curto em um grafo ponderado. O código é bastante complexo e comentado para facilitar o entendimento:

```haskell
import Data.List (minimumBy)
import Data.Maybe (fromMaybe)

type Vertice = Int
type Peso = Int
type Grafo = [(Vertice, [(Vertice, Peso)])]

-- Função auxiliar para encontrar o menor valor de uma lista de tuplas
minTupla :: (Ord b) => [(a, b)] -> (a, b)
minTupla [] = error "Lista vazia!"
minTupla [x] = x
minTupla (x:xs)
    | snd x < snd menorResto = x
    | otherwise = menorResto
    where menorResto = minTupla xs

-- Função auxiliar para atualizar o valor de um vértice em uma lista de tuplas
atualizaVertice :: (Eq a) => a -> b -> [(a, b)] -> [(a, b)]
atualizaVertice _ _ [] = []
atualizaVertice v novoValor ((x, valor):xs) 
    | v == x = (v, novoValor) : xs
    | otherwise = (x, valor) : atualizaVertice v novoValor xs

-- Função para encontrar o caminho mais curto utilizando o algoritmo de Dijkstra
dijkstra :: Grafo -> Vertice -> Vertice -> [Vertice]
dijkstra grafo origem destino = dijkstraAux [(origem, 0)] []
    where
        dijkstraAux :: [(Vertice, Peso)] -> [Vertice] -> [Vertice]
        dijkstraAux distancias visitados
            | origem == destino = [origem]
            | null naoVisitados = []
            | verticeAtual == destino = reconstruirCaminho origem destino predecessores
            | otherwise = dijkstraAux (atualizarDistancias distancias vizinhos) (verticeAtual:visitados)
            where
                naoVisitados = filter (\(v, _) -> not (elem v visitados)) distancias
                (verticeAtual, _) = minTupla naoVisitados
                vizinhos = fromMaybe [] (lookup verticeAtual grafo)
                atualizarDistancias dists [] = dists
                atualizarDistancias dists ((v, peso):vs) =
                    atualizarDistancias (atualizaVertice v novoValor dists) vs
                    where
                        novoValor = fromMaybe maxBound (lookup verticeAtual dists) + peso
                predecessores = foldl (\acc (v, _) -> atualizaVertice v verticeAtual acc) [] vizinhos
                reconstruirCaminho o d preds = reverse (d:reconstruirCaminhoAux o d preds)
                reconstruirCaminhoAux o d preds
                    | d == o = [o]
                    | otherwise = d : reconstruirCaminhoAux o pred preds
                    where
                        pred = fromMaybe undefined (lookup d preds)

-- Exemplo de uso
meuGrafo :: Grafo
meuGrafo = [(1, [(2, 5), (3, 2)]), (2, [(3, 1), (4, 6)]), (3, [(4, 1)]), (4, [])]

main :: IO ()
main = do
    let origem = 1
        destino = 4
        caminhoMaisCurto = dijkstra meuGrafo origem destino
    putStrLn $ "Caminho mais curto de " ++ show origem ++ " a " ++ show destino ++ ": " ++ show caminhoMaisCurto
```

Neste código, definimos um tipo `Grafo` que representa um grafo ponderado, onde cada vértice é um número inteiro e cada aresta possui um peso. A função `dijkstra` implementa o algoritmo de Dijkstra para encontrar o caminho mais curto entre dois vértices em um grafo.

A função `dijkstraAux` é uma função auxiliar recursiva que recebe como parâmetros a lista de distâncias até cada vértice, a lista de vértices visitados até o momento e retorna o caminho mais curto encontrado até o destino. A cada iteração, selecionamos o vértice não visitado com a menor distância e atualizamos as distâncias dos vizinhos desse vértice. Também mantemos uma lista de predecessores para reconstruir o caminho mais curto no final.

No exemplo de uso, definimos um grafo de exemplo `meuGrafo` e chamamos a função `dijkstra` para encontrar o caminho mais curto entre o vértice 1 e o vértice 4. O resultado é impresso na tela.

Espero que este código atenda às suas expectativas!