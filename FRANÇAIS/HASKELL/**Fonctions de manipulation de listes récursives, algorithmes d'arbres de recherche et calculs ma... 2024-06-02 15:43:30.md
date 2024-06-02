**Fonctions de manipulation de liste récursives**

```haskell
-- Renvoie le premier élément d'une liste
premier :: [a] -> a
premier (x:_) = x

-- Renvoie tous les éléments d'une liste sauf le premier
reste :: [a] -> [a]
reste (_:xs) = xs

-- Concatène deux listes
concat :: [a] -> [a] -> [a]
concat [] ys = ys
concat xs [] = xs
concat (x:xs) ys = x : concat xs ys

-- Inverse une liste
renverser :: [a] -> [a]
renverser [] = []
renverser (x:xs) = renverser xs ++ [x]

-- Trie une liste en utilisant l'algorithme de tri par insertion
trier :: Ord a => [a] -> [a]
trier [] = []
trier (x:xs) = inserer x (trier xs)

-- Insère un élément dans une liste triée
inserer :: Ord a => a -> [a] -> [a]
inserer e [] = [e]
inserer e (x:xs)
  | e <= x = e : x : xs
  | otherwise = x : inserer e xs
```

**Type et algorithme de recherche d'arbre**

```haskell
data Arbre a = Feuille | Noeud (Arbre a) a (Arbre a)
  deriving (Read, Show)

-- Recherche un élément dans un arbre
rechercher :: Ord a => a -> Arbre a -> Maybe a
rechercher e Feuille = Nothing
rechercher e (Noeud g v d)
  | e == v = Just v
  | e < v = rechercher e g
  | otherwise = rechercher e d
```

**Calcul de fonctions mathématiques**

```haskell
-- Calcule la factorielle d'un nombre
factorielle :: Int -> Int
factorielle 0 = 1
factorielle n = n * factorielle (n - 1)

-- Calcule le sinus d'un angle en degrés
sinus :: Double -> Double
sinus x = sin (x * pi / 180)

-- Calcule la valeur absolue d'un nombre
absolu :: Double -> Double
absolu x
  | x >= 0 = x
  | otherwise = -x
```

**Syntaxe de modèle**

```haskell
-- Définit un modèle pour décomposer les paires
data Paire a b = Paire a b

-- Utilisation du modèle pour extraire les composants d'une paire
let (x, y) = Paire 1 2
```

**Expressions de compréhension**

```haskell
-- Génère une liste de nombres de 1 à 10
nombres = [x | x <- [1..10]]

-- Génère une liste de paires de nombres de 1 à 3
paires = [(x, y) | x <- [1..3], y <- [1..3]]
```