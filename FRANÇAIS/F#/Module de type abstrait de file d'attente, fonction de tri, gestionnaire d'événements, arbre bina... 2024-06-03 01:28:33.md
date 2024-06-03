**Module de type abstrait de file d'attente**

Ce module définit un type abstrait de file d'attente (FIFO) et ses opérations associées, en utilisant la programmation fonctionnelle.

```f#
type FileQueue<'a> with
    member this.IsEmpty = match this with | Queue _ -> false | _ -> true
    member this.Head = match this with | Queue (head, tail) -> head | _ -> failwith "File vide"
    static member Empty() = Null
    static member Enqueue(head, tail) = Queue(head, tail)
    member this.Dequeue() =
        match this with
        | Queue (head, tail) -> (head, match tail with
                                  | Queue tail -> tail
                                  | Null -> failwith "File vide"
                                  )
        | Null -> failwith "File vide"
```

**Fonction de tri**

Cette fonction trie une liste d'éléments en utilisant l'algorithme de tri rapide.

```f#
let rec triRapide liste =
    match liste with
    | [] -> []
    | tete :: queue ->
        let pivot = tete
        let (plusPetit, plusGrand) = List.partition (fun x -> x < pivot) queue
        triRapide plusPetit @ [pivot] @ triRapide plusGrand
```

**Gestionnaire d'événements**

Ce module définit un gestionnaire d'événements qui permet de s'abonner à des événements et de les déclencher.

```f#
module GestionnaireEvenements =
    let abonnes = Dictionary<string, List<'a>>()
    let déclencher evt args =
        List.iter (fun f -> f args) (abonnes.[evt])
    let s'abonner evt f = abonnes.[evt] <- f :: List.default [] abonnes.[evt]
    let seDésabonner evt f = abonnes.[evt] <- List.filter (not = f) abonnes.[evt]
```

**Arbre binaire**

Ce module définit un type abstrait d'arbre binaire et ses opérations associées.

```f#
type Arbre<'a> with
    member this.EstVide = match this with | Vide -> true | _ -> false
    member this.Racine = match this with | Noeud (r, _, _) -> r | Vide -> failwith "Arbre vide"
    member this.Gauche = match this with | Noeud (_, g, _) -> g | Vide -> failwith "Arbre vide"
    member this.Droit = match this with | Noeud (_, _, d) -> d | Vide -> failwith "Arbre vide"
    static member Vide() = Vide
    static member Noeud(r, g, d) = Noeud(r, g, d)
```

**Fonction de recherche de profondeur**

Cette fonction effectue une recherche de profondeur dans un arbre binaire.

```f#
let rec rechercheProfondeur arbre =
    match arbre with
    | Noeud (r, g, d) ->
        r + rechercheProfondeur g + rechercheProfondeur d
    | Vide -> 0
```