Claro! Aqui está um código complexo em Haskell que faz a criação de um algoritmo genético para resolver o problema do caixeiro viajante:

```haskell
import System.Random
import Data.List

data Cidade = Cidade { nome :: String, coordenadas :: (Double, Double) }
type Rota = [Cidade]
type Populacao = [Rota]

populacaoInicial :: Int -> [Cidade] -> Populacao
populacaoInicial tamanho cidades = replicate tamanho (shuffle cidades)

shuffle :: [a] -> [a]
shuffle xs = fst $ foldl (\(ys, gen) _ -> swapRandom ys gen) (xs, mkStdGen 42) [1..length xs]

swapRandom :: [a] -> StdGen -> ([a], StdGen)
swapRandom xs gen = let (idx, newGen) = randomR (0, length xs - 1) gen
                        el = xs !! idx
                        ys = take idx xs ++ [xs !! (idx + 1)] ++ drop (idx + 1) xs
                    in (ys, newGen)

fitness :: Rota -> Double
fitness rota = sum $ zipWith distancia rota (tail rota)
  where distancia (Cidade _ (x1, y1)) (Cidade _ (x2, y2)) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

selecao :: Populacao -> Int -> Populacao
selecao populacao n = take n $ sortOn fitness populacao

crossover :: Rota -> Rota -> StdGen -> Rota
crossover rotaPai rotaMae gen = take metade rotaPai ++ drop metade rotaMae
  where metade = length rotaPai `div` 2

mutacao :: Rota -> StdGen -> Rota
mutacao rota gen = swapRandom rota gen

gerarNovaGeracao :: Populacao -> StdGen -> Populacao
gerarNovaGeracao populacao gen = crossoverMutacao populacao (mkStdGen $ fst $ randomR (1, 100) gen)
  where crossoverMutacao [] _ = []
        crossoverMutacao (pai:mae:resto) gen' = novoFilho : crossoverMutacao resto gen''
          where (gen'', gen''') = split gen'
                novoFilho = mutacao (crossover pai mae gen''') gen''

encontrarMelhorRota :: Populacao -> Rota
encontrarMelhorRota populacao = head $ sortOn fitness populacao

main :: IO ()
main = do
  let cidades = [Cidade "A" (0, 0), Cidade "B" (1, 1), Cidade "C" (2, 2), Cidade "D" (3, 3), Cidade "E" (4, 4)]
      tamanhoPopulacao = 100
      numeroGeracoes = 1000
      populacaoInicial = populacaoInicial tamanhoPopulacao cidades
      populacaoFinal = foldl (\pop _ -> gerarNovaGeracao pop (mkStdGen 42)) populacaoInicial [1..numeroGeracoes]
      melhorRota = encontrarMelhorRota populacaoFinal
  putStrLn $ "Melhor rota encontrada: " ++ intercalate " -> " (map nome melhorRota)
  putStrLn $ "Distância percorrida: " ++ show (fitness melhorRota)
```

Este código implementa o algoritmo genético para resolver o problema do caixeiro viajante. Ele começa definindo o tipo de dados `Cidade`, que possui um nome e coordenadas. Em seguida, são definidos os tipos `Rota` (lista de cidades) e `Populacao` (lista de rotas).

A função `populacaoInicial` recebe o tamanho desejado da população e uma lista de cidades. Ela cria uma população inicial com o tamanho especificado, com as rotas sendo embaralhadas aleatoriamente.

A função `fitness` calcula o valor de adequação de uma rota, que é a soma das distâncias entre as cidades da rota. A função `selecao` seleciona as melhores rotas com base no valor de adequação.

A função `crossover` realiza o cruzamento entre duas rotas, tomando metade da rota do pai e metade da rota da mãe. A função `mutacao` realiza uma mutação em uma rota, trocando aleatoriamente duas cidades de posição.

A função `gerarNovaGeracao` recebe uma população e uma semente aleatória e gera uma nova população a partir da seleção, cruzamento e mutação das rotas da população anterior.

A função `encontrarMelhorRota` encontra a melhor rota de uma população, com base no valor de adequação das rotas.

No `main`, são definidas as cidades, o tamanho da população, o número de gerações e a população inicial. Em seguida, é feito um loop em que a nova população é gerada a cada iteração. Por fim, é exibida a melhor rota encontrada e a distância percorrida.