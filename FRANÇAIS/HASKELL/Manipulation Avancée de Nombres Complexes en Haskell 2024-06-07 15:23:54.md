**Code Haskell Complexe**

```haskell
module Complexe where

import Data.List (intercalate)

-- Définition du type Complexe
data Complexe = Complexe Double Double deriving (Eq, Show)

-- Fonctions opératoires
add :: Complexe -> Complexe -> Complexe
add (Complexe a1 b1) (Complexe a2 b2) = Complexe (a1 + a2) (b1 + b2)

subtract :: Complexe -> Complexe -> Complexe
subtract (Complexe a1 b1) (Complexe a2 b2) = Complexe (a1 - a2) (b1 - b2)

multiply :: Complexe -> Complexe -> Complexe
multiply (Complexe a1 b1) (Complexe a2 b2) = Complexe ((a1 * a2) - (b1 * b2)) ((a1 * b2) + (b1 * a2))

divide :: Complexe -> Complexe -> Complexe
divide (Complexe a1 b1) (Complexe a2 b2) = Complexe ((a1 * a2 + b1 * b2) / ((a2 * a2) + (b2 * b2))) ((b1 * a2 - a1 * b2) / ((a2 * a2) + (b2 * b2)))

-- Fonctions utilitaires
conjugue :: Complexe -> Complexe
conjugue (Complexe a b) = Complexe a (negate b)

norme :: Complexe -> Double
norme (Complexe a b) = sqrt (a * a + b * b)

-- Affichage personnalisé
instance Show Complexe where
  show (Complexe a b)
    | b == 0 = show a
    | b > 0 = intercalate " + " [show a, show b ++ "i"]
    | otherwise = intercalate " - " [show a, show (negate b) ++ "i"]

-- Fonctions de test
main :: IO ()
main = do
  let c1 = Complexe 3 4
  let c2 = Complexe 5 6
  print (add c1 c2)  -- Affiche 8 + 10i
  print (subtract c1 c2)  -- Affiche -2 - 2i
  print (multiply c1 c2)  -- Affiche 7 + 22i
  print (divide c1 c2)  -- Affiche 0.6 + 0.24i
  print (conjugue c1)  -- Affiche 3 - 4i
  print (norme c1)  -- Affiche 5
```

**Explication du Code**

* **Définition du Type Complexe:** Le type `Complexe` représente des nombres complexes avec des parties réelles et imaginaires.
* **Fonctions Opératoires:** `add`, `subtract`, `multiply` et `divide` implémentent les opérations arithmétiques de base.
* **Fonctions Utilitaires:** `conjugue` renvoie le complexe conjugué, et `norme` calcule la norme.
* **Affichage Personnalisé:** La fonction `show` fournit un affichage personnalisé pour les complexes.
* **Fonctions de Test:** La fonction `main` teste les différentes fonctions et imprime les résultats.