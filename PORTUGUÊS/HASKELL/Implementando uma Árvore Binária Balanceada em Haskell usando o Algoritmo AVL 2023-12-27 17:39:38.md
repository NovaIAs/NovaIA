Claro! Aqui está um código em Haskell que implementa uma árvore binária balanceada. A árvore é balanceada usando o algoritmo AVL (Árvore de Busca Binária Balanceada por Altura).

```haskell
module ArvoreBinaria where

data ArvoreBinaria a = Nulo | No a (ArvoreBinaria a) (ArvoreBinaria a)

altura :: ArvoreBinaria a -> Int
altura Nulo = 0
altura (No _ esq dir) = 1 + max (altura esq) (altura dir)

balanceamento :: ArvoreBinaria a -> Int
balanceamento Nulo = 0
balanceamento (No _ esq dir) = altura esq - altura dir

rotacaoEsquerda :: ArvoreBinaria a -> ArvoreBinaria a
rotacaoEsquerda (No valor esq (No dirValor dirEsq dirDir)) =
    No dirValor (No valor esq dirEsq) dirDir

rotacaoDireita :: ArvoreBinaria a -> ArvoreBinaria a
rotacaoDireita (No valor (No esqValor esqEsq esqDir) dir) =
    No esqValor esqEsq (No valor esqDir dir)

rotacaoEsquerdaDireita :: ArvoreBinaria a -> ArvoreBinaria a
rotacaoEsquerdaDireita (No valor esq dir) =
    rotacaoEsquerda (No valor (rotacaoDireita esq) dir)

rotacaoDireitaEsquerda :: ArvoreBinaria a -> ArvoreBinaria a
rotacaoDireitaEsquerda (No valor esq dir) =
    rotacaoDireita (No valor esq (rotacaoEsquerda dir))

inserir :: (Ord a) => a -> ArvoreBinaria a -> ArvoreBinaria a
inserir valor Nulo = No valor Nulo Nulo
inserir valor (No noValor esq dir)
    | valor < noValor = balancear (No noValor (inserir valor esq) dir)
    | valor > noValor = balancear (No noValor esq (inserir valor dir))
    | otherwise = No noValor esq dir
    where
        balancear arvore
            | balanceamento arvore > 1 && valor < noValor =
                rotacaoDireita arvore
            | balanceamento arvore < -1 && valor > noValor =
                rotacaoEsquerda arvore
            | balanceamento arvore > 1 && valor > noValor =
                rotacaoEsquerdaDireita arvore
            | balanceamento arvore < -1 && valor < noValor =
                rotacaoDireitaEsquerda arvore
            | otherwise = arvore

busca :: (Ord a) => a -> ArvoreBinaria a -> Bool
busca _ Nulo = False
busca valor (No noValor esq dir)
    | valor < noValor = busca valor esq
    | valor > noValor = busca valor dir
    | otherwise = True

preOrdem :: ArvoreBinaria a -> [a]
preOrdem Nulo = []
preOrdem (No valor esq dir) = [valor] ++ preOrdem esq ++ preOrdem dir

emOrdem :: ArvoreBinaria a -> [a]
emOrdem Nulo = []
emOrdem (No valor esq dir) = emOrdem esq ++ [valor] ++ emOrdem dir

posOrdem :: ArvoreBinaria a -> [a]
posOrdem Nulo = []
posOrdem (No valor esq dir) = posOrdem esq ++ posOrdem dir ++ [valor]
```

Neste código, a função `ArvoreBinaria` define o tipo de dados para uma árvore binária. O tipo `a` é paramétrico e pode ser substituído por qualquer tipo desejado. A estrutura da árvore é definida como um `No` que contém um valor e duas subárvores, uma esquerda e outra direita. O valor `Nulo` representa uma árvore vazia.

A função `altura` retorna a altura da árvore, enquanto a função `balanceamento` calcula o fator de balanceamento da árvore em um determinado nó. As funções `rotacaoEsquerda`, `rotacaoDireita`, `rotacaoEsquerdaDireita` e `rotacaoDireitaEsquerda` são usadas para realizar as rotações necessárias para balancear a árvore.

A função `inserir` é usada para inserir um novo valor na árvore. Ela chama a função `balancear` para garantir que a árvore esteja balanceada após a inserção. A função `busca` verifica se um determinado valor está presente na árvore.

As funções `preOrdem`, `emOrdem` e `posOrdem` são usadas para percorrer a árvore em pré-ordem, em ordem e em pós-ordem, respectivamente. Elas retornam listas com os valores dos nós visitados.

Esse código implementa uma árvore binária balanceada usando o algoritmo AVL, o que garante que as operações de inserção e busca tenham complexidade O(log n) em média.