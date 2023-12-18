Claro! Aqui está um exemplo de um código em F# que implementa um algoritmo de ordenação chamado Merge Sort. O Merge Sort é um algoritmo eficiente que divide a lista em sublistas menores, ordena cada sublista e, em seguida, mescla as sublistas ordenadas para obter a lista final ordenada.

```fsharp
open System

let rec mergeSort (lst: 'a list) (comparer: 'a -> 'a -> int) : 'a list =
    let rec merge (left: 'a list) (right: 'a list) : 'a list =
        match left, right with
        | [], _ -> right
        | _, [] -> left
        | x::xs, y::ys ->
            if comparer x y <= 0 then
                x :: merge xs (y::ys)
            else
                y :: merge (x::xs) ys

    let length = List.length lst
    if length <= 1 then
        lst
    else
        let middle = length / 2
        let left = mergeSort (List.take middle lst) comparer
        let right = mergeSort (List.skip middle lst) comparer
        merge left right

let lst = [5; 2; 9; 1; 7]
let sortedLst = mergeSort lst compare

printfn "Lista ordenada: %A" sortedLst
```

Neste código, a função `mergeSort` implementa o algoritmo de ordenação Merge Sort. Ela recebe como entrada uma lista (`lst`) e um comparador de elementos (`comparer`) que determina a ordem de classificação. O tipo genérico `'a` é usado para permitir a ordenação de diferentes tipos de elementos.

A função `mergeSort` é recursiva. Se a lista de entrada tiver tamanho igual ou inferior a 1, ela é retornada diretamente. Caso contrário, a lista é dividida em duas partes aproximadamente iguais (esquerda e direita) e a função `mergeSort` é chamada recursivamente em cada parte.

A função `merge` é usada para mesclar as sublistas ordenadas. Ela recebe duas listas (`left` e `right`) e retorna uma nova lista resultante da mesclagem. O padrão de correspondência (`match`) é usado para tratar os diferentes casos: se uma das listas for vazia, a outra lista é retornada diretamente; caso contrário, o primeiro elemento de cada lista é comparado usando o comparador fornecido. O elemento menor é adicionado à lista resultante e a função é chamada recursivamente com as listas restantes.

No exemplo final, uma lista de números `[5; 2; 9; 1; 7]` é ordenada usando o comparador padrão `compare`. O resultado é exibido no console usando a função `printfn`.