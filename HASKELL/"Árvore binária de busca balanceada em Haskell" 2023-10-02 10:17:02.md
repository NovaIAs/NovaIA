Claro, vou criar um código complexo em Haskell para você. Aqui está um exemplo de um programa que implementa uma árvore binária de busca balanceada:

```haskell
data Arvore a = Folha | No (Arvore a) a (Arvore a) deriving Show

inserir :: Ord a => a -> Arvore a -> Arvore a
inserir x Folha = No Folha x Folha
inserir x (No esq y dir)
    | x < y = balancear (inserir x esq) y dir
    | x > y = balancear esq y (inserir x dir)
    | otherwise = No esq y dir

balancear :: Arvore a -> a -> Arvore a -> Arvore a
balancear esq x dir
    | altura esq - altura dir > 1 = rotacaoDir (No esq x dir)
    | altura dir - altura esq > 1 = rotacaoEsq (No esq x dir)
    | otherwise = No esq x dir

rotacaoDir :: Arvore a -> Arvore a
rotacaoDir (No (No esq1 x dir1) y dir2) = No esq1 x (No dir1 y dir2)
rotacaoDir _ = error "Erro de rotação direita"

rotacaoEsq :: Arvore a -> Arvore a
rotacaoEsq (No esq1 x (No esq2 y dir2)) = No (No esq1 x esq2) y dir2
rotacaoEsq _ = error "Erro de rotação esquerda"

altura :: Arvore a -> Int
altura Folha = 0
altura (No esq _ dir) = 1 + max (altura esq) (altura dir)

buscar :: Ord a => a -> Arvore a -> Bool
buscar _ Folha = False
buscar x (No esq y dir)
    | x < y = buscar x esq
    | x > y = buscar x dir
    | otherwise = True

main :: IO ()
main = do
    let arvore = foldr inserir Folha [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    putStrLn "Árvore:"
    print arvore
    putStrLn "Altura da árvore:"
    print (altura arvore)
    putStrLn "Buscar 5 na árvore:"
    print (buscar 5 arvore)
    putStrLn "Buscar 11 na árvore:"
    print (buscar 11 arvore)
```

Neste código, definimos o tipo de dados `Arvore` que representa uma árvore binária de busca balanceada. A função `inserir` é responsável por inserir um elemento na árvore, mantendo-a balanceada. A função `balancear` verifica se a árvore precisa ser balanceada e realiza as rotações necessárias. As funções `rotacaoDir` e `rotacaoEsq` realizam as rotações para a direita e esquerda, respectivamente. A função `altura` calcula a altura da árvore. Por fim, a função `buscar` verifica se um elemento está presente na árvore. No `main`, criamos uma árvore com os valores de 1 a 10 e realizamos algumas operações, como imprimir a árvore, calcular sua altura e buscar elementos.