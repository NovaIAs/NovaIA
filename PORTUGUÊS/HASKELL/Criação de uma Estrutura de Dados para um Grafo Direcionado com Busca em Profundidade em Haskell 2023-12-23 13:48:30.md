Claro! Aqui está um código complexo em Haskell que cria uma estrutura de dados para representar um grafo direcionado e implementa o algoritmo de busca em profundidade (DFS) para encontrar um caminho entre dois vértices:

```haskell
module Grafo where

import Data.Maybe
import Data.List

data Vertice = Vertice { valor :: Int, visitado :: Bool } deriving (Show, Eq)

data Aresta = Aresta { origem :: Vertice, destino :: Vertice } deriving (Show, Eq)

data Grafo = Grafo { vertices :: [Vertice], arestas :: [Aresta] } deriving (Show)

inserirVertice :: Grafo -> Vertice -> Grafo
inserirVertice g v = g { vertices = v : (vertices g) }

inserirAresta :: Grafo -> Aresta -> Grafo
inserirAresta g a = g { arestas = a : (arestas g) }

buscarVertice :: Grafo -> Int -> Maybe Vertice
buscarVertice g v = find (\v' -> valor v' == v) (vertices g)

buscarAresta :: Grafo -> Vertice -> Vertice -> Maybe Aresta
buscarAresta g u v = find (\a -> origem a == u && destino a == v) (arestas g)

dfs :: Grafo -> Int -> Int -> [Vertice]
dfs g u v = dfs' g (fromJust (buscarVertice g u)) (fromJust (buscarVertice g v)) []

dfs' :: Grafo -> Vertice -> Vertice -> [Vertice] -> [Vertice]
dfs' _ _ destino@(Vertice valorDestino _) visitados | valorDestino == valor visitadosVertice = visitadosVertice : visitados
  where visitadosVertice = Vertice valorDestino True
dfs' g atual@(Vertice valorAtual _) destino visitados
  | visitado atual = visitados
  | otherwise = foldl' (\acc v -> dfs' g v destino acc) (atual : visitados) vizinhosNaoVisitados
  where visitadosAtual = Vertice valorAtual True
        vizinhos = [destino a | a <- arestas g, origem a == atual]
        vizinhosNaoVisitados = filter (\v -> not (visitado v)) vizinhos
```

Explicação:
1. A estrutura de dados do grafo é definida utilizando tipos de dados algébricos. Um grafo é composto por uma lista de vértices e uma lista de arestas, onde cada vértice possui um valor e uma flag indicando se já foi visitado.
2. As funções `inserirVertice` e `inserirAresta` permitem adicionar vértices e arestas ao grafo, respectivamente.
3. As funções `buscarVertice` e `buscarAresta` são auxiliares para buscar vértices e arestas no grafo a partir de seus valores.
4. A função `dfs` é o ponto de entrada para o algoritmo de busca em profundidade (DFS). Ela recebe o grafo, o valor do vértice de origem e o valor do vértice de destino, e retorna uma lista com os vértices visitados durante a busca.
5. A função `dfs'` é a função recursiva que realiza a busca em profundidade. Ela recebe o grafo, o vértice atual, o vértice de destino e a lista de vértices visitados até o momento. A função verifica se o vértice atual é o vértice de destino e, se sim, adiciona-o à lista de vértices visitados. Caso contrário, ela continua a busca pelos vértices vizinhos não visitados, recursivamente.
6. O algoritmo utiliza a função `foldl'` para aplicar a função `dfs'` a todos os vértices vizinhos não visitados do vértice atual, acumulando a lista de vértices visitados.

Espero que essa implementação atenda às suas expectativas!