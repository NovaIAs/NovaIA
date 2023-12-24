Claro! Vou criar um código complexo em Haskell para você. Este código é um exemplo de um algoritmo de busca em largura em um grafo direcionado. Vou explicar cada parte do código em detalhes:

```haskell
import Data.List (nub)

-- Definindo o tipo Grafo como uma lista de pares de vértices
type Grafo = [(Int, [Int])]

-- Função auxiliar para buscar todos os vértices vizinhos de um vértice dado
vizinhos :: Grafo -> Int -> [Int]
vizinhos grafo v = case lookup v grafo of
  Just vizinhos -> vizinhos
  Nothing       -> []

-- Função de busca em largura
buscaEmLargura :: Grafo -> Int -> [Int]
buscaEmLargura grafo inicio = buscaEmLargura' [(inicio, [inicio])] []
  where
    buscaEmLargura' :: [(Int, [Int])] -> [Int] -> [Int]
    buscaEmLargura' [] visitados = visitados
    buscaEmLargura' ((vertice, caminho):resto) visitados
      | vertice `elem` visitados = buscaEmLargura' resto visitados
      | otherwise = buscaEmLargura' (resto ++ novosVizinhos) (visitados ++ [vertice])
      where
        vizinhosVertice = vizinhos grafo vertice
        novosVizinhos = [(v, caminho ++ [v]) | v <- vizinhosVertice]

-- Função principal
main :: IO ()
main = do
  let grafo = [(1, [2, 3]), (2, [4]), (3, [5]), (4, [3]), (5, [1])]
  let resultado = buscaEmLargura grafo 1
  putStrLn $ "Resultado da busca em largura: " ++ show resultado
```

Aqui está o código em Haskell que implementa um algoritmo de busca em largura em um grafo direcionado. O código utiliza a biblioteca `Data.List` para a função `nub`, que remove elementos duplicados de uma lista.

O tipo `Grafo` é definido como uma lista de pares de vértices, onde cada elemento é um par onde o primeiro elemento é o vértice e o segundo elemento é uma lista de vértices vizinhos.

A função `vizinhos` é uma função auxiliar que recebe um grafo e um vértice e retorna uma lista com todos os vértices vizinhos do vértice dado. A função utiliza a função `lookup` para buscar os vizinhos do vértice no grafo.

A função `buscaEmLargura` é a função principal que realiza a busca em largura no grafo. Ela recebe o grafo e o vértice de início da busca. A função utiliza uma lista de tuplas `(Int, [Int])` para representar os vértices visitados e os caminhos percorridos até cada vértice. A função utiliza uma função auxiliar `buscaEmLargura'` para realizar a busca em largura de fato.

A função `buscaEmLargura'` recebe a lista de vértices a serem visitados e a lista de vértices já visitados. Ela utiliza a técnica de busca em largura para percorrer todos os vértices do grafo. A função utiliza uma lista `resto` para armazenar os vértices a serem visitados e uma lista `novosVizinhos` para armazenar os novos vizinhos de um vértice visitado. A função utiliza o padrão de casamento `| vertice `elem` visitados` para verificar se um vértice já foi visitado.

A função principal `main` cria um exemplo de grafo e realiza a busca em largura a partir do vértice 1. O resultado da busca é impresso na tela utilizando a função `putStrLn`.

Espero que esse código complexo em Haskell atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, estou à disposição para ajudar.