**Module de calcul d'éléments finis**

```haskell
{-# LANGUAGE GADTs, RankNTypes #-}

import Data.List
import Data.Semigroup


-- Type des éléments finis
data Element fini =
    -- Élément à un nœud
    Node Int (Double, Double)
    | -- Élément à deux nœuds
    Bar Int Int (Double, Double) (Double, Double)
    deriving (Eq, Ord, Show)


-- Type des mailles d'éléments finis
data Maille fini =
    Maille
        { noeuds :: [Int] -- Numéros des nœuds
        , elements :: [Element fini] -- Éléments finis
        }
    deriving (Show)


-- Fonction de création d'une maille
creerMaille :: [Int] -> [Element fini] -> Maille fini
creerMaille noeuds elements =
    Maille
        { noeuds = noeuds
        , elements = elements
        }


-- Fonction de calcul de la matrice de rigidité d'une maille
calculMatriciteRigidite :: Maille fini -> [[Double]]
calculMatriciteRigidite maille =
    let
        -- Matrice de rigidité initiale
        matriceRigidite = zeros (length (noeuds maille)) (length (noeuds maille))

        -- Ajout des rigidités des éléments à la matrice
        matriceRigidite' = foldl' (\m e -> ajouterRigidite e m) matriceRigidite (elements maille)
    in
        matriceRigidite'


-- Fonction d'ajout de la rigidité d'un élément à une matrice
ajouterRigidite :: Element fini -> [[Double]] -> [[Double]]
ajouterRigidite element matrice =
    case element of
        -- Élément à un nœud
        Node nid (x, y) -> do
            let
                -- Indices des lignes et des colonnes
                i = nid - 1
                j = i

                -- Valeurs de la matrice
                ki = 2 * x^2 + y^2
                kj = 2 * y^2 + x^2
            modify (i, j) (ki +) matrice
            modify (j, i) (kj +) matrice

        -- Élément à deux nœuds
        Bar nid1 nid2 (x1, y1) (x2, y2) -> do
            let
                -- Indices des lignes et des colonnes
                i1 = nid1 - 1
                i2 = nid2 - 1
                j1 = i1
                j2 = i2

                -- Valeurs de la matrice
                k11 = x1^2 + y1^2
                k12 = -x1 * x2 + y1 * y2
                k21 = k12
                k22 = x2^2 + y2^2
            modify (i1, j1) (k11 +) matrice
            modify (i1, j2) (k12 +) matrice
            modify (i2, j1) (k21 +) matrice
            modify (i2, j2) (k22 +) matrice
    where
        -- Fonction de modification d'un élément de la matrice
        modify :: Int -> Int -> (Double -> Double) -> [[Double]] -> [[Double]]
        modify i j f matrice =
            let
                ligne = matrice !! i
            in
                (matrice !! i) `update` j (f (ligne !! j))


-- Exemple de création et de calcul de la matrice de rigidité d'une maille
-- Maille carrée avec les nœuds aux coins et un élément barre au milieu
exemple = do
    let
        noeuds = [1, 2, 3, 4]
        elements = [Node 1 (0.0, 0.0), Node 2 (1.0, 0.0), Node 3 (1.0, 1.0), Node 4 (0.0, 1.0), Bar 1 2 (0.5, 0.0) (0.5, 1.0)]
        maille = creerMaille noeuds elements
    calculMatriciteRigidite maille

-- Affichage de l'exemple
print exemple
```

**Explication du code**

Ce code implémente un module permettant de calculer la matrice de rigidité d'une maille d'éléments finis. La matrice de rigidité est une matrice carrée qui représente la rigidité de la maille dans toutes les directions.

**Type des éléments finis**

Le type `Element fini` représente les éléments finis qui composent la maille. Il existe deux types d'éléments :

* **`Node`** : un élément à un nœud, qui représente un point dans l'espace.
* **`Bar`** : un élément à deux nœuds, qui représente une barre reliant deux points.

**Type des mailles d'éléments finis**

Le type `Maille fini` représente les mailles d'éléments finis. Une maille est définie par les numéros des nœuds qui la composent et par les éléments finis qui la relient.

**Fonction de création d'une maille**

La fonction `creerMaille` permet de créer une maille à partir d'une liste de numéros de nœuds et d'une liste d'éléments finis.

**Fonction de calcul de la matrice de rigidité**

La fonction `calculMatriciteRigidite` calcule la matrice de rigidité d'une maille. Elle utilise la fonction `ajouterRigidite` pour ajouter la rigidité de chaque élément à la matrice.

**Fonction d'ajout de la rigidité d'un élément**

La fonction `ajouterRigidite` ajoute la rigidité d'un élément à une matrice. Elle utilise les coordonnées des nœuds de l'élément pour calculer les valeurs de la matrice de rigidité.

**Exemple**

L'exemple de code crée une maille carrée avec les nœuds aux coins et un élément barre au milieu. Il calcule ensuite la matrice de rigidité de cette maille et l'affiche.