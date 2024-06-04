**Définition d'un type de données algébrique pour représenter un arbre binaire**

```f#
type Arbre = Noeud of int * Arbre * Arbre | Feuile
```

**Fonction récursive pour calculer la somme des valeurs des nœuds d'un arbre**

```f#
let sommeArbre (arbre: Arbre) : int =
    match arbre with
    | Feuile -> 0
    | Noeud (v, g, d) -> v + sommeArbre g + sommeArbre d
```

**Fonction pour créer un arbre binaire à partir d'une liste de valeurs**

```f#
let creerArbre (liste: int list) : Arbre =
    let rec creerArbreRec (liste: int list) : Arbre =
        match liste with
        | [] -> Feuile
        | v :: reste -> Noeud (v, creerArbreRec reste, Feuile)
    creerArbreRec liste
```

**Fonction pour afficher un arbre binaire en profondeur**

```f#
let afficherArbre (arbre: Arbre) =
    let rec afficherArbreRec (arbre: Arbre, profondeur: int) =
        match arbre with
        | Feuile -> printfn "Feuille"
        | Noeud (v, g, d) ->
            printfn "%d" v
            afficherArbreRec g (profondeur + 1)
            afficherArbreRec d (profondeur + 1)
    afficherArbreRec arbre 0
```

**Utilisation des fonctions**

```f#
let arbre = creerArbre [1; 2; 3; 4; 5]
let somme = sommeArbre arbre
printfn "Somme des valeurs des noeuds : %d" somme
afficherArbre arbre
```

**Explication du code**

Le code définit un type de données algébrique pour représenter un arbre binaire, avec deux constructeurs : `Noeud` pour les nœuds internes et `Feuille` pour les feuilles.

La fonction `sommeArbre` calcule la somme des valeurs des nœuds d'un arbre en utilisant une récursivité.

La fonction `creerArbre` crée un arbre binaire à partir d'une liste de valeurs en utilisant une récursivité.

La fonction `afficherArbre` affiche un arbre binaire en profondeur à l'aide d'une récursivité.

Dans l'utilisation du code, un arbre binaire est créé à partir de la liste `[1; 2; 3; 4; 5]`. Ensuite, la somme des valeurs des nœuds est calculée et affichée. Enfin, l'arbre est affiché en profondeur.