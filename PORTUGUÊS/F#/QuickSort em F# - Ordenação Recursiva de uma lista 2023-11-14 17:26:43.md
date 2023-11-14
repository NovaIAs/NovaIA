Claro! Aqui está um código complexo em F# que faz a ordenação de uma lista de maneira recursiva utilizando o algoritmo QuickSort:

```fsharp
open System

let rec quicksort lista =
    match lista with
    | [] -> []
    | pivot :: tail ->
        let lesser, greater = List.partition (fun x -> x <= pivot) tail
        quicksort lesser @ (pivot :: quicksort greater)

let imprimeLista lista =
    printfn "Lista ordenada:"
    List.iter (printfn "%d") lista

[<EntryPoint>]
let main argv =
    let listaDesordenada = [5; 1; 9; 2; 10; 4; 3; 8; 7; 6]
    printfn "Lista desordenada:"
    List.iter (printfn "%d") listaDesordenada
    let listaOrdenada = quicksort listaDesordenada
    imprimeLista listaOrdenada
    0
```

Explicação do código:

O código começa importando o namespace System para utilizar funções de entrada e saída padrão. Em seguida, declaramos uma função `quicksort` que recebe uma lista como parâmetro.

A função `quicksort` utiliza uma expressão `match` para verificar se a lista está vazia. Se estiver, retorna uma lista vazia. Caso contrário, a função seleciona o primeiro elemento da lista como pivô e o remove utilizando o operador `::`. Em seguida, a função divide a lista restante em duas partes: uma com elementos menores ou iguais ao pivô, e outra com elementos maiores.

Utilizando a função `List.partition`, a lista de elementos restantes é dividida em duas: `lesser`, que contém os elementos menores ou iguais ao pivô, e `greater`, que contém os elementos maiores. Essas duas partes são ordenadas recursivamente chamando a própria função `quicksort` e, por fim, concatenadas utilizando o operador `@`.

Após a definição da função `quicksort`, declaramos uma função `imprimeLista` que recebe uma lista como parâmetro e imprime todos os elementos dela utilizando a função `printfn`.

Em seguida, declaramos a função `main` que é o ponto de entrada do programa. Dentro dessa função, criamos uma lista desordenada chamada `listaDesordenada` e a exibimos na tela utilizando a função `printfn`.

Chamamos então a função `quicksort` passando a `listaDesordenada` como parâmetro para obter a lista ordenada. Após isso, chamamos a função `imprimeLista` para exibir a lista ordenada na tela.

No final, o programa retorna 0 para indicar que foi executado com sucesso.