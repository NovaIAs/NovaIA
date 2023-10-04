Claro! Aqui está um exemplo de um código complexo em F# que utiliza programação funcional para criar uma função recursiva que calcula o fatorial de um número:

```fsharp
let rec fatorial n =
    if n = 0 then
        1
    else
        n * fatorial (n - 1)

let rec fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

let rec bubbleSort arr =
    let rec bubbleSortIter arr =
        match arr with
        | x::y::xs when x > y ->
            y :: bubbleSortIter (x::xs)
        | x::y::xs ->
            x :: bubbleSortIter (y::xs)
        | _ ->
            arr
    
    match bubbleSortIter arr with
    | x::xs ->
        let sortedTail = bubbleSort xs
        if x > List.head sortedTail then
            List.head sortedTail :: bubbleSort (x :: List.tail sortedTail)
        else
            x :: sortedTail
    | _ ->
        arr

let rec quickSort arr =
    match arr with
    | [] ->
        []
    | pivot::xs ->
        let smaller, larger = List.partition (fun x -> x < pivot) xs
        quickSort smaller @ [pivot] @ quickSort larger

let rec mergeSort arr =
    let rec merge left right =
        match left, right with
        | [], _ -> right
        | _, [] -> left
        | x::xs, y::ys ->
            if x < y then
                x :: merge xs right
            else
                y :: merge left ys
    
    match arr with
    | [] | [_] ->
        arr
    | _ ->
        let half = List.length arr / 2
        let left = List.take half arr
        let right = List.skip half arr
        merge (mergeSort left) (mergeSort right)
```

Neste código, temos várias funções distintas. A primeira função `fatorial` é uma função recursiva que calcula o fatorial de um número. Ela verifica se o número é igual a zero e, caso seja, retorna 1. Caso contrário, ela multiplica o número pelo fatorial do número anterior.

A segunda função `fibonacci` também é recursiva e calcula o n-ésimo número da sequência de Fibonacci. Ela utiliza casamento de padrões (`match`) para verificar se o número é igual a 0 ou 1 e retorna os valores correspondentes. Caso contrário, ela chama a função `fibonacci` recursivamente para o número anterior e o número anterior ao anterior e soma os resultados.

As funções `bubbleSort`, `quickSort` e `mergeSort` são algoritmos de ordenação. O `bubbleSort` implementa o algoritmo de ordenação bolha, o `quickSort` implementa o algoritmo de ordenação rápida e o `mergeSort` implementa o algoritmo de ordenação por mesclagem. Cada uma dessas funções é recursiva e utiliza casamento de padrões para separar a lista em elementos menores e maiores em relação a um pivô (no caso do `quickSort`) ou para dividir a lista em metades (no caso do `mergeSort) e então chama a função recursivamente para ordenar as partes menores e maiores antes de combiná-las.