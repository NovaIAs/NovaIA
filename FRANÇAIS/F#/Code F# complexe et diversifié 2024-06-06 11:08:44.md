**Code F# complexe et différencié**

```f#
open System
open System.Collections.Generic
open System.Linq

// Fonction de tri rapide
let rec tri_rapide liste =
    match liste with
    | [] -> []
    | x :: xs ->
        let (gauche, droite) = List.partition (< x) xs
        tri_rapide gauche @ [x] @ tri_rapide droite

// Fonction de recherche binaire
let rec recherche_binaire liste elem =
    let rec aux gauche droite =
        if gauche = droite then
            -1 // Élément non trouvé
        else
            let m = (gauche + droite) / 2
            match compare liste.[m] elem with
            | 0 -> m // Élément trouvé
            | -1 -> aux (m + 1) droite
            | 1 -> aux gauche m
    aux 0 (liste.Length - 1)

// Fonction pour trouver les permutations d'une liste
let rec permutations liste =
    match liste with
    | [] -> [[]]
    | x :: xs ->
        let perms = permutations xs
        let nouvelles_perms = List.map (fun p -> x :: p) perms
        perms @ nouvelles_perms

// Fonction pour générer le produit cartésien de deux listes
let produit_cartesien l1 l2 =
    seq {
        for x in l1 do
            for y in l2 do
                yield x, y
    }

// Fonction pour créer un arbre binaire de recherche
let creer_arbre liste =
    if liste.Length = 0 then
        null
    else
        let n = liste.Length
        let m = n / 2
        let gauche = creer_arbre (List.take m liste)
        let droite = creer_arbre (List.skip (m + 1) liste)
        Node(liste.[m], gauche, droite)

// Fonction pour effectuer un parcours en profondeur d'un arbre
let rec parcours_profondeur arbre =
    match arbre with
    | null -> []
    | Node(v, g, d) ->
        v :: parcours_profondeur g @ parcours_profondeur d
```

**Explication des fonctions**

* **`tri_rapide`** : implémente l'algorithme de tri rapide.
* **`recherche_binaire`** : effectue une recherche binaire dans une liste triée.
* **`permutations`** : génère toutes les permutations d'une liste.
* **`produit_cartesien`** : génère le produit cartésien de deux listes.
* **`creer_arbre`** : crée un arbre binaire de recherche à partir d'une liste.
* **`parcours_profondeur`** : effectue un parcours en profondeur d'un arbre binaire.