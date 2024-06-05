**Code en Haskell**

```haskell
-- Définition de type pour représenter les arbres binaires de recherche

data Arbre b = Vide
             | Noeud b (Arbre b) (Arbre b)
              deriving (Show, Eq)

-- Fonction pour insérer un élément dans un arbre binaire de recherche

insérer :: Ord b => b -> Arbre b -> Arbre b
insérer x Vide = Noeud x Vide Vide
insérer x (Noeud y g d)
  | x < y    = Noeud y (insérer x g) d
  | x > y    = Noeud y g (insérer x d)
  | otherwise = Noeud x g d

-- Fonction pour supprimer un élément dans un arbre binaire de recherche

supprimer :: Ord b => b -> Arbre b -> Arbre b
supprimer x Vide = Vide
supprimer x (Noeud y g d)
  | x < y    = Noeud y (supprimer x g) d
  | x > y    = Noeud y g (supprimer x d)
  | otherwise = fusionner g d

-- Fonction auxiliaire pour fusionner deux arbres binaires de recherche

fusionner :: Arbre b -> Arbre b -> Arbre b
fusionner Vide t = t
fusionner t Vide = t
fusionner (Noeud x g d) (Noeud y g' d')
  | x < y    = Noeud x g (fusionner d (Noeud y g' d'))
  | otherwise = Noeud y (fusionner (Noeud x g d) g') d'

-- Fonction pour rechercher un élément dans un arbre binaire de recherche

rechercher :: Ord b => b -> Arbre b -> Bool
rechercher x Vide = False
rechercher x (Noeud y g d)
  | x == y   = True
  | x < y    = rechercher x g
  | otherwise = rechercher x d

-- Fonction pour trouver le minimum d'un arbre binaire de recherche

minimum :: Arbre b -> b
minimum (Noeud x Vide _) = x
minimum (Noeud _ g _) = minimum g

-- Fonction pour trouver le maximum d'un arbre binaire de recherche

maximum :: Arbre b -> b
maximum (Noeud x _ Vide) = x
maximum (Noeud _ _ d) = maximum d

-- Fonction pour imprimer un arbre binaire de recherche dans l'ordre croissant

imprimerCroissant :: Arbre b -> String
imprimerCroissant Vide = ""
imprimerCroissant (Noeud x g d) = imprimerCroissant g ++ show x ++ " " ++ imprimerCroissant d
```

**Explication du code**

Ce code Haskell implémente les opérations courantes sur les arbres binaires de recherche, notamment l'insertion, la suppression, la recherche, la recherche du minimum et du maximum, ainsi que l'impression dans l'ordre croissant.

Voici une explication de chaque fonction :

* **`insérer` :** Cette fonction insère un élément dans un arbre binaire de recherche en utilisant la récursion. Elle compare l'élément à insérer avec la valeur du nœud actuel et insère l'élément dans la sous-arborescence de gauche ou de droite selon que l'élément est inférieur ou supérieur à la valeur du nœud.

* **`supprimer` :** Cette fonction supprime un élément d'un arbre binaire de recherche en utilisant la récursion. Elle compare l'élément à supprimer avec la valeur du nœud actuel et supprime l'élément de la sous-arborescence de gauche ou de droite selon que l'élément est inférieur ou supérieur à la valeur du nœud. Si l'élément à supprimer est égal à la valeur du nœud, la fonction fusionne les sous-arbres gauche et droit pour maintenir la structure de l'arbre binaire de recherche.

* **`fusionner` :** Cette fonction fusionne deux arbres binaires de recherche en un seul arbre binaire de recherche. Elle compare les valeurs des nœuds racines des deux arbres et fusionne les sous-arbres gauche et droit en conséquence.

* **`rechercher` :** Cette fonction recherche un élément dans un arbre binaire de recherche en utilisant la récursion. Elle compare l'élément à rechercher avec la valeur du nœud actuel et recherche l'élément dans la sous-arborescence de gauche ou de droite selon que l'élément est inférieur ou supérieur à la valeur du nœud.

* **`minimum` :** Cette fonction trouve le minimum d'un arbre binaire de recherche en trouvant l'élément le plus à gauche de l'arbre.

* **`maximum` :** Cette fonction trouve le maximum d'un arbre binaire de recherche en trouvant l'élément le plus à droite de l'arbre.

* **`imprimerCroissant` :** Cette fonction imprime les éléments d'un arbre binaire de recherche dans l'ordre croissant en utilisant la récursion. Elle imprime d'abord les éléments de la sous-arborescence de gauche, puis la valeur du nœud actuel, puis les éléments de la sous-arborescence de droite.