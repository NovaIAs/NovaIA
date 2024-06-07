**Algorithme de tri par fusion en Haskell**

```haskell
fusion :: [Int] -> [Int] -> [Int]
fusion [] [] = []
fusion [] (x:xs) = x:xs
fusion (x:xs) [] = x:xs
fusion (x:xs) (y:ys)
  | x <= y = x : fusion xs (y:ys)
  | otherwise = y : fusion (x:xs) ys

triFusion :: [Int] -> [Int]
triFusion [] = []
triFusion [x] = [x]
triFusion xs =
  let (gauche, droite) = split xs
  in fusion (triFusion gauche) (triFusion droite)
  where
    split :: [Int] -> ([Int], [Int])
    split xs = (take (length xs `div` 2) xs, drop (length xs `div` 2) xs)
```

**Explication du code**

L'algorithme de tri par fusion fonctionne en divisant une liste en deux parties égales, en triant récursivement chaque partie et en fusionnant les deux parties triées en une seule liste triée.

**Fonction `fusion`**

La fonction `fusion` prend deux listes triées comme arguments et renvoie une seule liste triée. Elle compare les premiers éléments des deux listes et ajoute l'élément le plus petit à la liste résultante. Elle répète ce processus jusqu'à ce que l'une des listes soit vide.

**Fonction `triFusion`**

La fonction `triFusion` prend une liste comme argument et renvoie une liste triée.

**Fonction `split`**

La fonction `split` divise une liste en deux parties égales. Elle utilise la fonction `take` pour prendre les `n` premiers éléments d'une liste et la fonction `drop` pour supprimer les `n` premiers éléments d'une liste, où `n` est la moitié de la longueur de la liste.