Claro! Aqui está um exemplo de código em Haskell que implementa uma árvore binária de busca e realiza a busca de um elemento na árvore:

```haskell
data ArvoreBinaria a = Nulo | No a (ArvoreBinaria a) (ArvoreBinaria a) deriving Show

inserir :: Ord a => a -> ArvoreBinaria a -> ArvoreBinaria a
inserir x Nulo = No x Nulo Nulo
inserir x (No y esq dir)
    | x < y = No y (inserir x esq) dir
    | x > y = No y esq (inserir x dir)
    | otherwise = No x esq dir

buscar :: Ord a => a -> ArvoreBinaria a -> Bool
buscar _ Nulo = False
buscar x (No y esq dir)
    | x < y = buscar x esq
    | x > y = buscar x dir
    | otherwise = True

main :: IO ()
main = do
    let arvore = foldr inserir Nulo [4, 2, 7, 1, 3, 6, 9]
    putStrLn "Digite um número para buscar na árvore:"
    numero <- readLn
    if buscar numero arvore
        then putStrLn "O número está presente na árvore."
        else putStrLn "O número não está presente na árvore."
```

Neste código, começamos definindo o tipo de dado `ArvoreBinaria`, que representa uma árvore binária de busca. Em seguida, temos a função `inserir`, que insere um elemento na árvore de forma ordenada. A função `buscar` realiza a busca de um elemento na árvore.

No `main`, criamos uma árvore inicial com alguns elementos e solicitamos ao usuário um número para buscar na árvore. A função `readLn` é usada para ler o número digitado pelo usuário. Em seguida, chamamos a função `buscar` para verificar se o número está presente na árvore e exibimos uma mensagem correspondente.

Espero que esse código atenda às suas expectativas!