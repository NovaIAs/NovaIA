**Module de Tri**

```haskell
module Tri where

import Data.List (sort)

-- Tri à bulles
triBulles :: Ord a => [a] -> [a]
triBulles [] = []
triBulles [x] = [x]
triBulles (x:xs) = minimum xs : triBulles (filter (/= minimum xs) xs)

-- Tri à insertion
triInsertion :: Ord a => [a] -> [a]
triInsertion [] = []
triInsertion (x:xs) = insert x (triInsertion xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x <= y = x : y : ys
  | otherwise = y : insert x ys

-- Tri rapide
triRapide :: Ord a => [a] -> [a]
triRapide [] = []
triRapide [x] = [x]
triRapide (x:xs) = triRapide (filter (< x) xs) ++ [x] ++ triRapide (filter (>= x) xs)

-- Tri par fusion
triFusion :: Ord a => [a] -> [a]
triFusion [] = []
triFusion [x] = [x]
triFusion xs = merge (triFusion (take (length xs `div` 2) xs)) (triFusion (drop (length xs `div` 2) xs))

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
```

**Module de Recherche**

```haskell
module Recherche where

import Data.List (elemIndex)

-- Recherche linéaire
rechercheLineaire :: Eq a => a -> [a] -> Maybe Int
rechercheLineaire x xs = elemIndex x xs

-- Recherche binaire
rechercheBinaire :: Ord a => a -> [a] -> Maybe Int
rechercheBinaire x xs = binaire xs 0 (length xs - 1)

binaire :: Ord a => [a] -> Int -> Int -> Maybe Int
binaire [] _ _ = Nothing
binaire (x:_) i j
  | i == j = if x == y then Just i else Nothing
  | i > j = Nothing
  | x == y = Just i
  | autrement =
      let m = (i + j) `div` 2
      in binaire xs (i + 1) m
      <|> binaire xs (m + 1) j
  where
    y = xs !! i
```

**Module de Manipulation de Listes**

```haskell
module Listes where

import Data.List (nub, sort, zip)

-- Supprimer les doublons
supprimerDoublons :: Eq a => [a] -> [a]
supprimerDoublons = nub

-- Trier une liste
trier :: Ord a => [a] -> [a]
trier = sort

-- Zipper deux listes
zipper :: [a] -> [b] -> [(a, b)]
zipper xs ys
  | length xs == length ys = zip xs ys
  | autrement = error "Les listes doivent avoir la même longueur."
```

**Exemple d'Utilisation**

```haskell
import Tri
import Recherche
import Listes

main :: IO ()
main = do
  let xs = [5, 2, 7, 3, 1, 9]
  print $ triBulles xs
  print $ triInsertion xs
  print $ triRapide xs
  print $ triFusion xs
  print $ rechercheLineaire 3 xs
  print $ rechercheBinaire 3 xs
  print $ supprimerDoublons xs
  print $ trier xs
  print $ zipper xs [1, 2, 3, 4, 5]
```