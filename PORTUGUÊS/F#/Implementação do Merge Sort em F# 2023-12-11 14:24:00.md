Claro! Aqui está um exemplo de um código em F# que implementa um algoritmo de classificação chamado Merge Sort. O Merge Sort é um algoritmo de ordenação eficiente que divide a lista em sub-listas menores, ordena cada sub-lista e, em seguida, mescla as sub-listas ordenadas para obter a lista final ordenada.

```fsharp
module MergeSort

let rec merge (left: int list) (right: int list) =
    match left, right with
    | [], _ -> right
    | _, [] -> left
    | lh :: lt, rh :: rt ->
        if lh <= rh then
            lh :: (merge lt right)
        else
            rh :: (merge left rt)

let rec mergeSort (list: int list) =
    match list with
    | [] -> []
    | [x] -> [x]
    | _ ->
        let half = list.Length / 2
        let left = list.[0..half-1]
        let right = list.[half..]
        merge (mergeSort left) (mergeSort right)

[<EntryPoint>]
let main argv =
    let numbers = [10; 5; 3; 8; 2; 7; 1; 6; 4; 9]
    let sortedNumbers = mergeSort numbers
    printfn "Lista original: %A" numbers
    printfn "Lista ordenada: %A" sortedNumbers
    0
```

Neste código, eu defino duas funções principais: `merge` e `mergeSort`. A função `merge` recebe duas listas (`left` e `right`) e as mescla em uma única lista ordenada. A função `mergeSort` recebe uma lista e a divide em duas partes, chamando recursivamente `mergeSort` em cada parte e, em seguida, mescla as partes ordenadas usando `merge`.

No bloco `<EntryPoint>`, eu criei uma lista de números desordenados e chamei a função `mergeSort` passando essa lista. Em seguida, imprimo a lista original e a lista ordenada na saída.

Espero que isso atenda às suas expectativas!