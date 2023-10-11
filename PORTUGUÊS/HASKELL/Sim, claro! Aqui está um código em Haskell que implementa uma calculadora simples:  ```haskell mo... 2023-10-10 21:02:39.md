Claro! Aqui está um código em Haskell que implementa uma simulação de um jogo de tabuleiro chamado "Labirinto Mágico". Neste jogo, o objetivo é encontrar um tesouro em um labirinto cheio de armadilhas.

```haskell
import System.Random

-- Tipo de dado para representar o labirinto
data Labirinto = Labirinto {
    tamanho :: Int,   -- Tamanho do labirinto (quadrado)
    tesouro :: (Int, Int),   -- Posição do tesouro
    jogador :: (Int, Int),   -- Posição inicial do jogador
    armadilhas :: [(Int, Int)]   -- Posições das armadilhas
}

-- Função para criar um novo labirinto
criarLabirinto :: Int -> Int -> [Int] -> Labirinto
criarLabirinto tam semente armadilhas = Labirinto {
    tamanho = tam,
    tesouro = (tam-1, tam-1),
    jogador = (0, 0),
    armadilhas = gerarPosicoes tam semente armadilhas
}

-- Função para gerar as posições das armadilhas aleatoriamente
gerarPosicoes :: Int -> Int -> [Int] -> [(Int, Int)]
gerarPosicoes tam semente armadilhas = take (length armadilhas) $ zip rs1 rs2
    where
        rs1 = randomRs (0, tam-1) (mkStdGen semente)
        rs2 = randomRs (0, tam-1) (mkStdGen (semente + 1))

-- Função para verificar se uma posição está dentro do labirinto
posicaoValida :: Labirinto -> (Int, Int) -> Bool
posicaoValida labirinto (x, y) = x >= 0 && x < tam && y >= 0 && y < tam
    where
        tam = tamanho labirinto

-- Função para mover o jogador para uma nova posição
moverJogador :: Labirinto -> (Int, Int) -> Labirinto
moverJogador labirinto (x, y)
    | posicaoValida labirinto novaPosicao && novaPosicao /= tesouro labirinto = labirinto { jogador = novaPosicao }
    | otherwise = labirinto
    where
        (jx, jy) = jogador labirinto
        novaPosicao = (jx + x, jy + y)

-- Função para verificar se o jogador encontrou o tesouro
encontrouTesouro :: Labirinto -> Bool
encontrouTesouro labirinto = jogador labirinto == tesouro labirinto

-- Função principal do jogo
jogarLabirinto :: Labirinto -> IO ()
jogarLabirinto labirinto = do
    putStrLn "Bem-vindo ao Labirinto Mágico!"
    putStrLn "Encontre o tesouro para vencer o jogo."
    loopJogo labirinto

-- Função para loop do jogo
loopJogo :: Labirinto -> IO ()
loopJogo labirinto = do
    putStrLn $ "Você está na posição: " ++ show (jogador labirinto)
    putStrLn "Escolha uma direção para se mover:"
    putStrLn "1. Cima"
    putStrLn "2. Baixo"
    putStrLn "3. Esquerda"
    putStrLn "4. Direita"
    escolha <- getLine
    novoLabirinto <- case escolha of
        "1" -> return $ moverJogador labirinto (-1, 0)
        "2" -> return $ moverJogador labirinto (1, 0)
        "3" -> return $ moverJogador labirinto (0, -1)
        "4" -> return $ moverJogador labirinto (0, 1)
        _ -> do
            putStrLn "Escolha inválida. Tente novamente."
            loopJogo labirinto
    if encontrouTesouro novoLabirinto
        then putStrLn "Parabéns, você encontrou o tesouro e venceu o jogo!"
        else if jogador novoLabirinto `elem` armadilhas novoLabirinto
            then putStrLn "Você caiu em uma armadilha e perdeu o jogo!"
            else loopJogo novoLabirinto

-- Função principal
main :: IO ()
main = do
    let tamanhoLabirinto = 5
    let sementeAleatoria = 1234
    let posicoesArmadilhas = [1, 2, 3]
    let labirinto = criarLabirinto tamanhoLabirinto sementeAleatoria posicoesArmadilhas
    jogarLabirinto labirinto
```

Neste código, usamos o Haskell para criar uma estrutura de dados chamada `Labirinto`, que representa o estado do jogo. A função `criarLabirinto` é responsável por criar um novo labirinto com base no tamanho, semente aleatória e posições das armadilhas.

Em seguida, temos a função `posicaoValida`, que verifica se uma posição está dentro dos limites do labirinto. A função `moverJogador` permite mover o jogador para uma nova posição, desde que seja uma posição válida e não seja a posição do tesouro.

A função `encontrouTesouro` verifica se o jogador encontrou o tesouro no labirinto. A função principal do jogo é `jogarLabirinto`, que inicia o jogo e chama a função `loopJogo`, que é um loop principal onde o jogador pode escolher uma direção para se mover.

O código também inclui uma função `main`, onde são definidos o tamanho do labirinto, a semente aleatória e as posições das armadilhas. Em seguida, o labirinto é criado e o jogo é iniciado com a função `jogarLabirinto`.

Espero que este código atenda às suas expectativas!