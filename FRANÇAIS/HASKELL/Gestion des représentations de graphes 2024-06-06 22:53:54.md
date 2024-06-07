```haskell
-- Module principal du programme
module Main where

-- Importation de modules externes
import Data.List (nub, sort)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import System.Environment (getArgs)

-- Définition du type de donnée représentant un graphe orienté
data Graphe = Graphe
  { sommets :: [Char] -- Liste des sommets du graphe
  , arcs :: [(Char, Char, Int)] -- Liste des arcs du graphe
  }

-- Fonction auxiliaire pour créer un graphe à partir d'une liste de tuples
creerGraphe :: [(Char, Char, Int)] -> Graphe
creerGraphe arcs =
  Graphe
    { sommets = nub (concatMap fst arcs ++ concatMap snd arcs)
    , arcs = arcs
    }

-- Fonction auxiliaire pour convertir une représentation matricielle d'un graphe en une représentation par liste d'arcs
matriceToListeArcs :: [[Int]] -> [(Char, Char, Int)]
matriceToListeArcs matrice =
  [ ((sommet1 !! i), (sommet2 !! j), poids)
  | (i, sommet1) <- zip [0 ..] matrice
  , (j, sommet2) <- zip [0 ..] sommet1
  , poids <- matrice !! i !! j
  , poids /= Integer.maxBound
  ]
  where
    -- Récupération des sommets du graphe
    sommets = head matrice

-- Fonction auxiliaire pour convertir une représentation par liste d'arcs d'un graphe en une représentation matricielle
listeArcsToMatrice :: [(Char, Char, Int)] -> [[Int]]
listeArcsToMatrice arcs =
  [ [poids !! i !! j | j <- [0 .. n - 1]] | i <- [0 .. n - 1] ]
  where
    -- Récupération des sommets du graphe
    sommets = nub (concatMap fst arcs ++ concatMap snd arcs)
    -- Initialisation de la matrice avec des poids infinis
    matrice =
      [ [ Integer.maxBound | _ <- sommets ]
      | _ <- sommets
      ]
    -- Mise à jour des poids de la matrice
    poids =
      [ [ if (i, j) `elem` arcs then fromJust (lookup (i, j) arcs) else Integer.maxBound
        | j <- sommets
        ]
      | i <- sommets
      ]

-- Fonction auxiliaire pour afficher un graphe
afficherGraphe :: Graphe -> IO ()
afficherGraphe graphe = do
  putStrLn "Sommets : "
  putStrLn (show (sommets graphe))
  putStrLn "Arcs : "
  putStrLn (show (arcs graphe))

-- Fonction principale du programme
main :: IO ()
main = do
  -- Récupération des arguments de la ligne de commande
  args <- getArgs
  -- Vérification du nombre d'arguments
  case args of
    [nomFichier] -> do
      -- Lecture du fichier contenant la matrice d'adjacence du graphe
      matrice <- readFile nomFichier
      -- Conversion de la matrice en liste d'arcs
      arcs <- return (matriceToListeArcs (lines matrice))
      -- Création du graphe à partir de la liste d'arcs
      graphe <- return (creerGraphe arcs)
      -- Affichage du graphe
      afficherGraphe graphe
    _ -> do
      -- Message d'erreur si le nombre d'arguments est incorrect
      putStrLn "Usage : main <nom-fichier>"
```

**Explication du code :**

* Le code Haskell définit un type de données `Graphe` représentant un graphe orienté avec une liste de sommets et une liste d'arcs.
* Il fournit des fonctions auxiliaires pour créer un graphe à partir d'une liste d'arcs, convertir entre des représentations matricielles et par liste d'arcs, et afficher un graphe.
* La fonction principale lit une matrice d'adjacence d'un fichier, la convertit en une liste d'arcs, crée un graphe à partir de la liste d'arcs et l'affiche.
* Le code est complexe car il gère des représentations matricielles et par liste d'arcs de graphes, ce qui nécessite des fonctions de conversion et des vérifications approfondies.