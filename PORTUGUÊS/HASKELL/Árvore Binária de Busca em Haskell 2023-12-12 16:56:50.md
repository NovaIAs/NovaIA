Claro! Aqui está um código em Haskell que usa o conceito de arvore binária de busca para criar uma estrutura de dados que armazena números inteiros, permitindo a inserção, remoção e busca por elementos:

```haskell
-- Definição do tipo de dado para representar a árvore binária de busca
data Arvore = Nulo | No Int Arvore Arvore

-- Função para inserir um elemento na árvore
inserir :: Int -> Arvore -> Arvore
inserir x Nulo = No x Nulo Nulo
inserir x (No valor esquerda direita)
  | x == valor = No valor esquerda direita
  | x < valor = No valor (inserir x esquerda) direita
  | x > valor = No valor esquerda (inserir x direita)

-- Função para remover um elemento da árvore
remover :: Int -> Arvore -> Arvore
remover _ Nulo = Nulo
remover x (No valor esquerda direita)
  | x < valor = No valor (remover x esquerda) direita
  | x > valor = No valor esquerda (remover x direita)
  | otherwise = combinar esquerda direita
  where
    combinar :: Arvore -> Arvore -> Arvore
    combinar Nulo direita = direita
    combinar esquerda Nulo = esquerda
    combinar esquerda direita = No menorDireita esquerda novaDireita
      where
        menorDireita = minimo direita
        novaDireita = remover menorDireita direita
        minimo :: Arvore -> Int
        minimo (No valor Nulo _) = valor
        minimo (No _ esq _) = minimo esq

-- Função para buscar um elemento na árvore
buscar :: Int -> Arvore -> Bool
buscar _ Nulo = False
buscar x (No valor esquerda direita)
  | x == valor = True
  | x < valor = buscar x esquerda
  | x > valor = buscar x direita

-- Função auxiliar para imprimir a árvore
imprimirArvore :: Arvore -> String
imprimirArvore Nulo = ""
imprimirArvore (No valor esquerda direita) =
  "(" ++ imprimirArvore esquerda ++ show valor ++ imprimirArvore direita ++ ")"

-- Exemplo de uso
main :: IO ()
main = do
  let arvore = inserir 4 (inserir 2 (inserir 6 (inserir 1 (inserir 3 (inserir 5 Nulo)))))
  putStrLn $ "Árvore: " ++ imprimirArvore arvore
  putStrLn $ "Contém o número 3? " ++ show (buscar 3 arvore)
  putStrLn $ "Contém o número 7? " ++ show (buscar 7 arvore)
  let novaArvore = remover 2 arvore
  putStrLn $ "Árvore após a remoção do número 2: " ++ imprimirArvore novaArvore
```

Neste código, a função `inserir` recebe um número inteiro e uma árvore binária de busca, e insere o número na árvore de forma ordenada. A função `remover` remove um elemento da árvore, enquanto a função `buscar` verifica se um elemento está presente na árvore.

O tipo de dado `Arvore` é definido como sendo uma árvore binária de busca, onde cada nó possui um valor inteiro, uma subárvore esquerda e uma subárvore direita. O valor `Nulo` representa uma árvore vazia.

A função `imprimirArvore` é usada para imprimir a árvore de forma legível.

No exemplo de uso no final do código, uma árvore é criada inserindo alguns números e, em seguida, é verificado se os números 3 e 7 estão presentes na árvore. Depois, é removido o número 2 da árvore e é impressa a árvore resultante.

Espero que este código atenda às suas expectativas!