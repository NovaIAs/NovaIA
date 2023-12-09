Claro! Aqui está um exemplo de um código complexo em F# que realiza a classificação de elementos em uma lista usando o algoritmo QuickSort:

```fsharp
open System

let rec quicksort lista =
    match lista with
    | [] -> []
    | pivo :: resto ->
        let menores = List.filter (fun x -> x <= pivo) resto
        let maiores = List.filter (fun x -> x > pivo) resto
        let ordenadosMenores = quicksort menores
        let ordenadosMaiores = quicksort maiores
        ordenadosMenores @ [pivo] @ ordenadosMaiores

let listaDesordenada = [4; 2; 9; 1; 7; 5; 3; 8; 6]
let listaOrdenada = quicksort listaDesordenada

printfn "Lista desordenada: %A" listaDesordenada
printfn "Lista ordenada: %A" listaOrdenada
```

Neste código, começamos importando o namespace `System` para utilizar a função `printfn` para exibir os resultados na tela. Em seguida, definimos a função `quicksort` que recebe uma lista e retorna a lista ordenada.

Dentro da função `quicksort`, utilizamos a construção `match` para realizar a correspondência de padrões. Primeiro, verificamos se a lista está vazia. Se estiver vazia, retornamos uma lista vazia também. Caso contrário, pegamos o primeiro elemento da lista como pivô e o resto da lista como `resto`.

Em seguida, utilizamos as funções `List.filter` para criar duas listas: uma contendo os elementos menores ou iguais ao pivô (`menores`) e outra contendo os elementos maiores que o pivô (`maiores`).

Após isso, chamamos recursivamente a função `quicksort` para ordenar as listas `menores` e `maiores`. Recebemos as listas ordenadas em `ordenadosMenores` e `ordenadosMaiores`.

Por fim, concatenamos as listas `ordenadosMenores`, `[pivo]` e `ordenadosMaiores` usando o operador `@` para obter a lista final ordenada.

No final do código, criamos uma lista desordenada `listaDesordenada` e chamamos a função `quicksort` passando essa lista como argumento. O resultado é armazenado na variável `listaOrdenada`.

Por fim, utilizamos a função `printfn` para exibir a lista desordenada e a lista ordenada na tela.