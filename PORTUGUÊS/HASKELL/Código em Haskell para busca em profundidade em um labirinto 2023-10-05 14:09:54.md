Claro! Aqui está um código em Haskell que cria uma estrutura de dados chamada "Labirinto" e implementa um algoritmo de busca em profundidade para encontrar um caminho entre duas células no labirinto.

```Haskell
-- Definindo o labirinto como uma matriz de células
type Labirinto = [[Celula]]

-- Definindo as possíveis células do labirinto
data Celula = Piso | Parede deriving (Eq, Show)

-- Função principal que recebe o labirinto, a posição inicial e a posição final
encontrarCaminho :: Labirinto -> (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
encontrarCaminho labirinto inicio fim = buscaEmProfundidade labirinto inicio [inicio] fim

-- Função recursiva de busca em profundidade
buscaEmProfundidade :: Labirinto -> (Int, Int) -> [(Int, Int)] -> (Int, Int) -> Maybe [(Int, Int)]
buscaEmProfundidade labirinto atual visitados fim
  | atual == fim = Just visitados
  | otherwise = case proximasCandidatas labirinto atual visitados of
      [] -> Nothing -- Não existem mais células candidatas a serem visitadas
      candidatas -> busca candidatas
  where
    busca [] = Nothing
    busca (c:cs) = case buscaEmProfundidade labirinto c (visitados ++ [c]) fim of
      Nothing -> busca cs
      Just caminho -> Just caminho

-- Função auxiliar que retorna as células vizinhas não visitadas
proximasCandidatas :: Labirinto -> (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
proximasCandidatas labirinto (i, j) visitados =
  [ (i+1, j) | i < length labirinto - 1 && labirinto!!(i+1)!!j /= Parede && (i+1, j) `notElem` visitados] ++
  [ (i-1, j) | i > 0 && labirinto!!(i-1)!!j /= Parede && (i-1, j) `notElem` visitados] ++
  [ (i, j+1) | j < length (head labirinto) - 1 && labirinto!!i!!(j+1) /= Parede && (i, j+1) `notElem` visitados] ++
  [ (i, j-1) | j > 0 && labirinto!!i!!(j-1) /= Parede && (i, j-1) `notElem` visitados]
```

Este código implementa um algoritmo de busca em profundidade em Haskell para encontrar um caminho válido entre duas células em um labirinto representado por uma matriz. O labirinto é definido como uma lista de listas de células, onde cada célula pode ser do tipo Piso ou Parede.

A função `encontrarCaminho` é a função principal que recebe o labirinto, a posição inicial e a posição final. Ela chama a função `buscaEmProfundidade` para realizar a busca em profundidade.

A função `buscaEmProfundidade` é uma função recursiva que recebe o labirinto, a posição atual, a lista de células já visitadas e a posição final. Ela verifica se a posição atual é igual à posição final e, se for, retorna a lista de células visitadas. Caso contrário, ela chama a função `proximasCandidatas` para obter as células vizinhas não visitadas e continua a busca em cada uma delas.

A função `proximasCandidatas` é uma função auxiliar que recebe o labirinto, a posição atual e a lista de células já visitadas. Ela retorna uma lista com as células vizinhas que são candidatas a serem visitadas, ou seja, que não são paredes e não foram visitadas anteriormente.

Espero que este código atenda às suas expectativas!