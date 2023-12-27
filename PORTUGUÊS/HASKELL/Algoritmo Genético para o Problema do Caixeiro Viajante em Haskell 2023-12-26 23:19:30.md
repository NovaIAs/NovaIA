Claro! Aqui está um código em Haskell que implementa um algoritmo genético para resolver o problema do Caixeiro Viajante:

```haskell
import System.Random (randomRIO)
import Data.List (permutations)

type Cidade = (Int, Int)
type Rota = [Cidade]

distancia :: Cidade -> Cidade -> Float
distancia (x1, y1) (x2, y2) = sqrt (fromIntegral ((x2-x1)^2 + (y2-y1)^2))

custoRota :: Rota -> Float
custoRota rota = sum $ zipWith distancia rota (tail rota)

gerarPopulacaoInicial :: [Cidade] -> Int -> IO [Rota]
gerarPopulacaoInicial cidades tamanhoPopulacao = do
    let todasRotas = permutations cidades
    indices <- gerarIndicesAleatorios tamanhoPopulacao (length todasRotas)
    return $ map (todasRotas !!) indices

gerarIndicesAleatorios :: Int -> Int -> IO [Int]
gerarIndicesAleatorios n max = sequence $ replicate n (randomRIO (0, max-1))

selecionarMelhoresRotas :: [Rota] -> Int -> [Rota]
selecionarMelhoresRotas rotas n = take n $ sortOn custoRota rotas

crossover :: Rota -> Rota -> IO Rota
crossover rota1 rota2 = do
    pontoCorte <- randomRIO (1, length rota1 - 2)
    let (inicio1, fim1) = splitAt pontoCorte rota1
    let (inicio2, fim2) = splitAt pontoCorte rota2
    return $ inicio1 ++ (filter (`notElem` inicio1) fim2)

mutacao :: Rota -> IO Rota
mutacao rota = do
    indice1 <- randomRIO (0, length rota - 1)
    indice2 <- randomRIO (0, length rota - 1)
    let cidade1 = rota !! indice1
    let cidade2 = rota !! indice2
    return $ trocarCidades rota indice1 cidade2 indice2 cidade1

trocarCidades :: Rota -> Int -> Cidade -> Int -> Cidade -> Rota
trocarCidades rota indice1 cidade1 indice2 cidade2 =
    take indice1 rota ++ [cidade2] ++ drop (indice1+1) (take indice2 rota) ++ [cidade1] ++ drop (indice2+1) rota

gerarNovaGeracao :: [Rota] -> Int -> IO [Rota]
gerarNovaGeracao rotas tamanhoPopulacao = do
    novasRotas <- sequence $ map crossover rotas (tail rotas)
    mutadas <- sequence $ map mutacao novasRotas
    return $ rotas ++ mutadas

resolverCaixeiroViajante :: [Cidade] -> Int -> Int -> IO Rota
resolverCaixeiroViajante cidades tamanhoPopulacao numGeracoes = do
    populacaoInicial <- gerarPopulacaoInicial cidades tamanhoPopulacao
    let melhorRota = head $ foldl (\populacao _ -> selecionarMelhoresRotas populacao 1) populacaoInicial [1..numGeracoes]
    return melhorRota
```

Neste código, um conjunto de cidades é representado por uma lista de coordenadas `(x, y)`. A função `distancia` calcula a distância euclidiana entre duas cidades. A função `custoRota` calcula o custo total de uma rota, somando as distâncias entre as cidades consecutivas.

A função `gerarPopulacaoInicial` gera uma população inicial de rotas aleatórias, dado um conjunto de cidades e um tamanho de população desejado. A função `gerarIndicesAleatorios` gera uma lista de índices aleatórios dentro de um intervalo.

A função `selecionarMelhoresRotas` seleciona as `n` melhores rotas de uma população, com base no custo da rota.

A função `crossover` realiza o crossover entre duas rotas. A rota é dividida em dois pontos de corte e as partes intermediárias são trocadas entre as rotas.

A função `mutacao` realiza uma mutação em uma rota, trocando duas cidades aleatórias.

A função `trocarCidades` realiza a troca de duas cidades em uma rota.

A função `gerarNovaGeracao` gera uma nova geração de rotas, realizando o crossover e mutação em cada uma delas.

Por fim, a função `resolverCaixeiroViajante` recebe um conjunto de cidades, o tamanho da população inicial e o número de gerações e retorna a melhor rota encontrada.

Este código utiliza técnicas de programação genética para encontrar uma solução aproximada para o problema do Caixeiro Viajante, onde é necessário encontrar a rota mais curta que passe por todas as cidades uma única vez. Ao longo das gerações, as rotas são combinadas e mutadas para melhorar o resultado.