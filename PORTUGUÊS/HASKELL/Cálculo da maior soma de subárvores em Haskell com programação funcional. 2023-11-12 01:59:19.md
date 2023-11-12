Com certeza, aqui está um código complexo em Haskell que utiliza o conceito de programação funcional:

```
-- Módulo que define uma árvore com valores inteiros
module Arvore (Arvore (..)) where

-- Definição da estrutura da árvore
data Arvore = Nodo Int Arvore Arvore | Folha deriving Show

-- Função que retorna a soma de todos os valores da árvore
somaArvore :: Arvore -> Int
somaArvore Folha = 0
somaArvore (Nodo valor esq dir) = valor + somaArvore esq + somaArvore dir

-- Função que retorna a profundidade da árvore
profundidadeArvore :: Arvore -> Int
profundidadeArvore Folha = 0
profundidadeArvore (Nodo _ esq dir) = 1 + max (profundidadeArvore esq) (profundidadeArvore dir)

-- Função que retorna a subárvore esquerda com a maior soma de valores
maiorSomaEsq :: Arvore -> Arvore
maiorSomaEsq Folha = Folha
maiorSomaEsq (Nodo _ esq dir)
  | somaArvore esq > somaArvore dir = esq
  | otherwise = dir

-- Função que retorna a subárvore direita com a maior soma de valores
maiorSomaDir :: Arvore -> Arvore
maiorSomaDir Folha = Folha
maiorSomaDir (Nodo _ esq dir)
  | somaArvore dir > somaArvore esq = dir
  | otherwise = esq

-- Função que retorna a soma dos valores das subárvores com a maior soma
somaMaiorSoma :: Arvore -> Int
somaMaiorSoma Folha = 0
somaMaiorSoma (Nodo _ esq dir) = somaArvore (maiorSomaEsq (Nodo 0 esq dir)) + somaArvore (maiorSomaDir (Nodo 0 esq dir))

-- Exemplo de uso
arvoreExemplo = Nodo 10 (Nodo 5 Folha Folha) (Nodo 15 (Nodo 20 Folha Folha) Folha)
maiorSomaTotal = somaMaiorSoma arvoreExemplo
```

Neste código, é definida uma estrutura de dados para a representação de uma árvore binária, onde cada nó contém um valor inteiro e duas subárvores (esquerda e direita). Em seguida, são implementadas funções para calcular a soma de todos os valores da árvore, a profundidade da árvore, a subárvore esquerda com a maior soma de valores, a subárvore direita com a maior soma de valores e a soma dos valores das subárvores com a maior soma.

O exemplo de uso apresenta a criação de uma árvore com alguns valores inseridos e a chamada da função `somaMaiorSoma`, que retorna a soma dos valores das subárvores com a maior soma.