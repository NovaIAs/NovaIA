Claro! Aqui está um exemplo de código em Haskell que implementa um jogo de adivinhação simples. O objetivo é adivinhar um número aleatório entre 1 e 100. O código é dividido em várias funções para melhor organização e clareza.

```haskell
import System.Random

-- Função principal que inicia o jogo
main :: IO ()
main = do
  putStrLn "Bem-vindo ao jogo de adivinhação!"
  numeroAleatorio <- gerarNumeroAleatorio
  jogo numeroAleatorio

-- Gera um número aleatório entre 1 e 100
gerarNumeroAleatorio :: IO Int
gerarNumeroAleatorio = randomRIO (1, 100)

-- Função recursiva que controla o jogo
jogo :: Int -> IO ()
jogo numeroAleatorio = do
  putStrLn "Digite um número entre 1 e 100:"
  palpite <- getLine
  case readMaybe palpite of
    Just p ->
      if p == numeroAleatorio
        then putStrLn "Parabéns! Você acertou!"
        else do
          putStrLn $ dica p numeroAleatorio
          jogo numeroAleatorio
    Nothing -> do
      putStrLn "Entrada inválida! Por favor, digite um número válido."
      jogo numeroAleatorio

-- Gera uma dica com base no palpite do jogador
dica :: Int -> Int -> String
dica palpite numeroAleatorio
  | palpite < numeroAleatorio = "Tente um número maior."
  | palpite > numeroAleatorio = "Tente um número menor."
  | otherwise = error "Isso não deveria acontecer!"

-- Função auxiliar para converter uma string para um Maybe Int
readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
  [(x, "")] -> Just x
  _ -> Nothing
```

Explicação do código:

1. Importamos o módulo `System.Random` para obter a função `randomRIO`, que nos permite gerar um número aleatório.

2. Definimos a função `main` como a função principal do programa. Ela exibe uma mensagem de boas-vindas e chama a função `gerarNumeroAleatorio` para obter o número a ser adivinhado. Em seguida, chama a função `jogo` com o número aleatório.

3. A função `gerarNumeroAleatorio` utiliza a função `randomRIO` para gerar um número aleatório entre 1 e 100.

4. A função `jogo` é a função recursiva que controla o jogo. Ela recebe o número aleatório como argumento e solicita ao jogador um palpite. Em seguida, verifica se o palpite é igual ao número aleatório. Se for, exibe uma mensagem de parabéns. Caso contrário, exibe uma dica com base no palpite e chama a função `jogo` novamente.

5. A função `dica` recebe o palpite do jogador e o número aleatório como argumentos e retorna uma dica. Se o palpite for menor que o número aleatório, a dica é "Tente um número maior". Se for maior, a dica é "Tente um número menor". Caso contrário, ocorre um erro, pois o palpite não deveria ser igual ao número aleatório.

6. A função `readMaybe` é uma função auxiliar que converte uma string em um `Maybe Int`. Ela utiliza a função `reads` para converter a string em uma lista de pares `(Int, String)`. Se a lista contiver apenas um elemento e a string restante for vazia, retorna `Just x`, onde `x` é o número convertido. Caso contrário, retorna `Nothing`, indicando uma conversão inválida.