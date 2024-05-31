```haskell

-- Définition de types
data Arbre a = Feuille a | Noeud (Arbre a) (Arbre a)

-- Fonctions de construction d'arbres
feuille :: a -> Arbre a
feuille a = Feuille a

noeud :: Arbre a -> Arbre a -> Arbre a
noeud gauche droite = Noeud gauche droite

-- Fonctions d'accès aux éléments d'un arbre
valeur :: Arbre a -> a
valeur (Feuille a) = a
valeur (Noeud _ droite) = valeur droite

gauche :: Arbre a -> Arbre a
gauche (Noeud gauche _) = gauche
gauche _ = error "L'arbre n'a pas de fils gauche"

droite :: Arbre a -> Arbre a
droite (Noeud _ droite) = droite
droite _ = error "L'arbre n'a pas de fils droit"

-- Fonctions de manipulation des arbres
inserer :: Ord a => a -> Arbre a -> Arbre a
inserer x (Feuille y) = Noeud (feuille x) (feuille y)
inserer x (Noeud gauche droite)
  | x <= valeur gauche = Noeud (inserer x gauche) droite
  | otherwise = Noeud gauche (inserer x droite)

-- Fonction de recherche dans l'arbre
rechercher :: Ord a => a -> Arbre a -> Bool
rechercher x (Feuille y) = x == y
rechercher x (Noeud gauche droite)
  | x == valeur gauche = True
  | x <= valeur gauche = rechercher x gauche
  | otherwise = rechercher x droite

-- Fonction de tri d'une liste en utilisant l'arbre
tri :: Ord a => [a] -> [a]
tri [] = []
tri [x] = [x]
tri (x:xs) = inserer x (tri xs)

-- Fonction principale
main :: IO ()
main = do
  -- Création d'un arbre
  arbre = noeud (feuille 1) (noeud (feuille 2) (feuille 3))

  -- Recherche d'un élément dans l'arbre
  putStrLn $ "L'élément 2 est-il présent dans l'arbre ?"
  if rechercher 2 arbre then putStrLn "Oui" else putStrLn "Non"

  -- Tri d'une liste en utilisant l'arbre
  liste = [5, 1, 4, 2, 3]
  putStrLn $ "Liste triée : " ++ show (tri liste)

```

**Explications du code :**

* Ce code définit un type d'arbre binaire de recherche (ABR) avec des données génériques.
* Les fonctions `feuille` et `noeud` permettent de construire des arbres.
* Les fonctions `valeur`, `gauche` et `droite` permettent d'accéder aux éléments d'un arbre.
* La fonction `inserer` insère un élément dans un ABR.
* La fonction `rechercher` recherche un élément dans un ABR.
* La fonction `tri` utilise un ABR pour trier une liste.
* La fonction `main` est la fonction principale qui crée un arbre, recherche un élément dans l'arbre et trie une liste.