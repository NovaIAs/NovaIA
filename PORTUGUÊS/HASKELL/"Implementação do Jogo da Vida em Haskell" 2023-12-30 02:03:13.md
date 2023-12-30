Claro! Aqui está um exemplo de um código em Haskell que realiza uma simulação de um jogo de tabuleiro chamado "Jogo da Vida". 

```haskell
-- Jogo da Vida

type Celula = (Int, Int)
type Tabuleiro = [Celula]

vizinhos :: Celula -> [Celula]
vizinhos (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x-1, y-1), (x+1, y-1), (x-1, y+1)]

contarVizinhos :: Tabuleiro -> Celula -> Int
contarVizinhos tab celula = length $ filter (`elem` tab) (vizinhos celula)

sobreviver :: Tabuleiro -> Celula -> Bool
sobreviver tab celula
  | numVizinhos == 2 || numVizinhos == 3 = True
  | otherwise = False
  where numVizinhos = contarVizinhos tab celula

reproduzir :: Tabuleiro -> Celula -> Bool
reproduzir tab celula
  | numVizinhos == 3 = True
  | otherwise = False
  where numVizinhos = contarVizinhos tab celula

proximaGeracao :: Tabuleiro -> Tabuleiro
proximaGeracao tab = [celula | celula <- todasCelulas, sobreviver tab celula || reproduzir tab celula]
  where todasCelulas = [(x, y) | x <- [-10..10], y <- [-10..10]]

gerarTabuleiroInicial :: Tabuleiro
gerarTabuleiroInicial = [(0, 0), (0, 1), (1, 0), (1, 1), (-1, 0), (0, -1)]

imprimirTabuleiro :: Tabuleiro -> IO ()
imprimirTabuleiro tab = mapM_ putStrLn [imprimirLinha y | y <- [-10..10]]
  where imprimirLinha y = [if (x, y) `elem` tab then '#' else '.' | x <- [-10..10]]
  
simularJogo :: Int -> Tabuleiro -> IO ()
simularJogo 0 tab = putStrLn "Fim do jogo."
simularJogo n tab = do
  putStrLn $ "Geração " ++ show (11-n) ++ ":"
  imprimirTabuleiro tab
  putStrLn ""
  simularJogo (n-1) (proximaGeracao tab)

main :: IO ()
main = do
  putStrLn "Bem-vindo ao Jogo da Vida!"
  putStrLn "Digite o número de gerações que deseja simular:"
  numGeracoes <- readLn
  simularJogo numGeracoes gerarTabuleiroInicial
```

Este código implementa a lógica do jogo "Jogo da Vida", criado pelo matemático John Conway. O jogo é uma simulação de células vivas e mortas em um tabuleiro bidimensional.

No código, temos os tipos `Celula` e `Tabuleiro`, que são apenas sinônimos para tuplas de inteiros. A função `vizinhos` recebe uma célula e retorna uma lista com suas células vizinhas. A função `contarVizinhos` conta quantos vizinhos vivos uma célula tem no tabuleiro. As funções `sobreviver` e `reproduzir` determinam se uma célula sobrevive ou é reproduzida na próxima geração, com base no número de vizinhos vivos.

A função `proximaGeracao` recebe um tabuleiro e retorna o tabuleiro da próxima geração, aplicando as regras do jogo para cada célula. A função `gerarTabuleiroInicial` cria um tabuleiro de exemplo com algumas células vivas. A função `imprimirTabuleiro` imprime o tabuleiro de forma visual. A função `simularJogo` executa a simulação do jogo por um determinado número de gerações.

No `main`, o programa solicita ao usuário o número de gerações que deseja simular e inicia o jogo. Cada geração é impressa no console e a simulação termina após o número de gerações especificado.

Espero que esse código complexo em Haskell atenda às suas expectativas!