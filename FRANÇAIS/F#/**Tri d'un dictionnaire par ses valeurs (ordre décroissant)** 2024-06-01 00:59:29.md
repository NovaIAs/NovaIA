**Tri d'un dictionnaire par ses valeurs (tri par ordre décroissant)**

```f#
let triParValeurDecroissant = (dict : Map<string, int>) =>
    dict
    |> Seq.sortWith (fun (k1,v1) (k2,v2) -> v1 > v2)

let dicTest = Map.ofSeq [("A", 5); ("B", 2); ("C", 1); ("D", 12); ("E", 3)]
let res = triParValeurDecroissant dicTest
```

**Implémentation d'un algorithme génétique en F#**

```f#
type Chromosome = { Genes : Seq<int> }

let fitness (ch : Chromosome) = ch.Genes.Sum()

let select (pool : Seq<Chromosome>) =
    seq {
        for c in pool do
            if Random.Next() < fitness c / pool.SumBy fitness then
                yield c
    }

let crossover (c1 : Chromosome) (c2 : Chromosome) =
    let len = c1.Genes.Length
    let split = Random.Next(len)
    { Genes = c1.Genes.Take(split) @ c2.Genes.Skip(split) }

let mutate (ch : Chromosome) =
    let r = Random.Next(ch.Genes.Length)
    { Genes = ch.Genes.Take(r) @ [Random.Next()] @ ch.Genes.Skip(r+1) }

let evolve (pool : Seq<Chromosome>) =
    Seq.init (pool.Length) (fun _ -> select pool)
    |> Seq.map (crossover (seq {pool}) (seq {pool}))
    |> Seq.map (fun ch -> mutate ch)
```

**Un parseur JSON en F#**

```f#
/// <summary>
/// Fonction auxiliaire pour extraire le contenu d'un objet.
/// </summary>
/// <param name="obj">L'objet à extraire.</param>
/// <returns>La valeur extraite si possible, sinon null.</returns>
let tryExtractValue (obj : JObject) = obj |> JObject.TryGetValue (Seq.head obj.Keys)

/// <summary>
/// Fonction récursive pour parser un objet JSON.
/// </summary>
/// <param name="jo"> L'objet JSON à parser.</param>
/// <returns> Un arbre de valeurs correspondant à l'objet JSON.</returns>
let parseJSON (jo : JObject) =
    /// Si l'objet est une feuille, on retourne sa valeur.
    match tryExtractValue jo with
    | Some x -> x
    /// Sinon, on créée un dictionnaire avec les clés et valeurs parsées.
    | _ -> jo.Keys.ToSeq()
           .Map (fun k -> k, parseJSON jo)
           .ToDictionary()
```