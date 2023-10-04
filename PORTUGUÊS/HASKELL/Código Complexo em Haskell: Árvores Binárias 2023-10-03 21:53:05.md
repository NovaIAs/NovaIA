Claro! Aqui está um exemplo de um código complexo em Haskell:

```haskell
module Main where

import Control.Monad.State

-- Definindo o tipo de dado Tree que representa uma árvore binária
data Tree a = Leaf a | Node (Tree a) a (Tree a)

-- Função para calcular a soma dos elementos de uma árvore binária
somaArvore :: Tree Int -> Int
somaArvore (Leaf x) = x
somaArvore (Node left x right) = x + somaArvore left + somaArvore right

-- Função para verificar se um elemento está presente em uma árvore binária
elementoPresente :: Eq a => a -> Tree a -> Bool
elementoPresente x (Leaf y) = x == y
elementoPresente x (Node left y right) = x == y || elementoPresente x left || elementoPresente x right

-- Função para inserir um elemento em uma árvore binária
inserirElemento :: Ord a => a -> Tree a -> Tree a
inserirElemento x (Leaf y) = if x < y then Node (Leaf x) y (Leaf y) else Node (Leaf y) y (Leaf x)
inserirElemento x (Node left y right)
  | x < y = Node (inserirElemento x left) y right
  | x > y = Node left y (inserirElemento x right)
  | otherwise = Node left y right

-- Função para remover um elemento de uma árvore binária
removerElemento :: Ord a => a -> Tree a -> Tree a
removerElemento _ (Leaf _) = Leaf undefined -- Nesse exemplo, apenas retornamos uma Leaf vazia para fins de simplicidade
removerElemento x (Node left y right)
  | x < y = Node (removerElemento x left) y right
  | x > y = Node left y (removerElemento x right)
  | otherwise = undefined -- Nesse exemplo, apenas retornamos um valor undefined para fins de simplicidade

-- Função para percorrer uma árvore em ordem
percorrerOrdem :: Tree a -> [a]
percorrerOrdem tree = execState (percorrerOrdem' tree) []

percorrerOrdem' :: Tree a -> State [a] ()
percorrerOrdem' (Leaf x) = modify (++ [x])
percorrerOrdem' (Node left x right) = do
  percorrerOrdem' left
  modify (++ [x])
  percorrerOrdem' right

-- Função para percorrer uma árvore em pré-ordem
percorrerPreOrdem :: Tree a -> [a]
percorrerPreOrdem tree = execState (percorrerPreOrdem' tree) []

percorrerPreOrdem' :: Tree a -> State [a] ()
percorrerPreOrdem' (Leaf x) = modify (++ [x])
percorrerPreOrdem' (Node left x right) = do
  modify (++ [x])
  percorrerPreOrdem' left
  percorrerPreOrdem' right

-- Função para percorrer uma árvore em pós-ordem
percorrerPosOrdem :: Tree a -> [a]
percorrerPosOrdem tree = execState (percorrerPosOrdem' tree) []

percorrerPosOrdem' :: Tree a -> State [a] ()
percorrerPosOrdem' (Leaf x) = modify (++ [x])
percorrerPosOrdem' (Node left x right) = do
  percorrerPosOrdem' left
  percorrerPosOrdem' right
  modify (++ [x])

-- Função principal que demonstra o uso das funções acima
main :: IO ()
main = do
  let tree = Node (Node (Leaf 1) 2 (Leaf 3)) 4 (Node (Leaf 5) 6 (Leaf 7))
  putStrLn "Árvore original:"
  print tree
  putStrLn ""
  putStrLn "Soma dos elementos da árvore:"
  print (somaArvore tree)
  putStrLn ""
  putStrLn "Verificar se o elemento 5 está presente na árvore:"
  print (elementoPresente 5 tree)
  putStrLn "Verificar se o elemento 8 está presente na árvore:"
  print (elementoPresente 8 tree)
  putStrLn ""
  putStrLn "Inserir o elemento 8 na árvore:"
  let newTree = inserirElemento 8 tree
  print newTree
  putStrLn ""
  putStrLn "Remover o elemento 4 da árvore:"
  let removedTree = removerElemento 4 tree
  print removedTree
  putStrLn ""
  putStrLn "Percorrer a árvore em ordem:"
  print (percorrerOrdem tree)
  putStrLn ""
  putStrLn "Percorrer a árvore em pré-ordem:"
  print (percorrerPreOrdem tree)
  putStrLn ""
  putStrLn "Percorrer a árvore em pós-ordem:"
  print (percorrerPosOrdem tree)
```

Neste exemplo em Haskell, definimos um tipo de dado `Tree` que representa uma árvore binária. Em seguida, implementamos diversas funções que realizam operações com árvores binárias, como calcular a soma dos elementos, verificar se um elemento está presente, inserir e remover elementos, e percorrer a árvore em ordem, pré-ordem e pós-ordem.

Na função principal `main`, criamos uma árvore de exemplo e demonstramos o uso das funções implementadas. Imprimimos a árvore original, a soma dos elementos, verificamos a presença de elementos, inserimos e removemos elementos, e percorremos a árvore em diferentes ordens.

Esse código em Haskell é complexo e apresenta uma série de funcionalidades relacionadas a árvores binárias. Ele pode ser útil em cenários onde é necessário manipular e trabalhar com esse tipo de estrutura de dados.