Claro! Aqui está um exemplo de código complexo em F# que aborda o algoritmo de ordenação chamado Merge Sort.

```fsharp
module MergeSort

let rec merge left right =
    match left, right with
    | [], right -> right
    | left, [] -> left
    | h1::t1, h2::t2 ->
        if h1 <= h2 then
            h1 :: (merge t1 right)
        else
            h2 :: (merge left t2)

let rec mergeSort list =
    let length = List.length list
    if length <= 1 then
        list
    else
        let middle = length / 2
        let left = List.take middle list
        let right = List.skip middle list
        let sortedLeft = mergeSort left
        let sortedRight = mergeSort right
        merge sortedLeft sortedRight

// Exemplo de uso
let lista = [5; 2; 1; 6; 4; 3]
let listaOrdenada = mergeSort lista

printfn "Lista original: %A" lista
printfn "Lista ordenada: %A" listaOrdenada
```

Este código implementa o algoritmo de ordenação Merge Sort em F#. O Merge Sort é um algoritmo eficiente para ordenar listas, que divide a lista original em sublistas menores, ordena cada sublista separadamente e, em seguida, mescla as sublistas ordenadas para produzir a lista final ordenada.

Neste código, temos duas funções principais: `merge` e `mergeSort`. A função `merge` recebe duas listas ordenadas e as mescla em uma única lista ordenada. A função `mergeSort` é a função principal que implementa a lógica do algoritmo. Ela divide a lista original em sublistas, chama recursivamente o `mergeSort` em cada sublista e, por fim, chama a função `merge` para mesclar as sublistas ordenadas.

No exemplo de uso fornecido no final do código, uma lista de números é criada e passada para a função `mergeSort`. Em seguida, a lista original e a lista ordenada são impressas no console.

Espero que esse exemplo tenha sido útil! Se precisar de mais ajuda, é só me perguntar.