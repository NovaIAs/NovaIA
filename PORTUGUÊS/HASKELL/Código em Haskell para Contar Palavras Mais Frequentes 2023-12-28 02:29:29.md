Claro! Aqui está um código complexo em Haskell, utilizando algumas funções avançadas e expressões lambda para realizar uma tarefa de processamento de texto:

```haskell
import Data.Char (isDigit, isAlpha)
import Data.List (groupBy, sortBy)
import Data.Function (on)

-- Função auxiliar para remover caracteres não alfanuméricos de uma string
removerCaracteresEspeciais :: String -> String
removerCaracteresEspeciais = filter (\c -> isAlpha c || isDigit c)

-- Função auxiliar para transformar uma string em uma lista de palavras
separarPalavras :: String -> [String]
separarPalavras = words . removerCaracteresEspeciais

-- Função auxiliar que conta a quantidade de ocorrências de uma palavra em uma lista de palavras
contarOcorrencias :: String -> [String] -> Int
contarOcorrencias palavra palavras = length $ filter (== palavra) palavras

-- Função principal que recebe um texto e retorna as palavras mais frequentes, ordenadas por frequência
palavrasMaisFrequentes :: String -> [(String, Int)]
palavrasMaisFrequentes texto =
  let palavras = separarPalavras $ map toLower texto
      agrupadas = groupBy ((==) `on` id) $ sortBy (compare `on` id) palavras
      contagens = map (\g -> (head g, contarOcorrencias (head g) palavras)) agrupadas
      ordenadas = sortBy (flip compare `on` snd) contagens
  in take 10 ordenadas
```

Neste código, temos uma função `removerCaracteresEspeciais` que remove os caracteres não alfanuméricos de uma string utilizando a função `filter` e expressões lambda.

A função `separarPalavras` utiliza a função `words` para transformar a string em uma lista de palavras e, em seguida, chama a função `removerCaracteresEspeciais` para filtrar os caracteres especiais.

A função `contarOcorrencias` recebe uma palavra e uma lista de palavras, utilizando a função `filter` para filtrar apenas as ocorrências da palavra e, em seguida, utiliza a função `length` para contar quantas ocorrências existem.

Por fim, a função `palavrasMaisFrequentes` recebe um texto e realiza as seguintes etapas:

1. Transforma o texto em minúsculas utilizando `map toLower`.
2. Separa as palavras utilizando a função `separarPalavras`.
3. Agrupa as palavras em listas com base no seu valor utilizando `groupBy` e `sortBy`.
4. Conta as ocorrências de cada palavra utilizando a função `contarOcorrencias`.
5. Ordena as palavras com base na sua frequência utilizando `sortBy`.
6. Retorna as 10 palavras mais frequentes.