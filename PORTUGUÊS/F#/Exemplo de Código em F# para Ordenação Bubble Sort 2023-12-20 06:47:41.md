Claro! Aqui está um exemplo de código em F# que implementa um algoritmo de ordenação chamado Bubble Sort:

```fsharp
module BubbleSort

let bubbleSort (array: int array) =
    let mutable swapped = true
    let mutable arr = array

    while swapped do
        swapped <- false
        for i in 0 .. arr.Length - 2 do
            if arr.[i] > arr.[i+1] then
                let temp = arr.[i]
                arr.[i] <- arr.[i+1]
                arr.[i+1] <- temp
                swapped <- true

    arr

let printArray (array: int array) =
    for i in 0 .. array.Length - 1 do
        printf "%d " array.[i]
    printfn ""

[<EntryPoint>]
let main argv =
    let array = [| 64; 34; 25; 12; 22; 11; 90 |]
    printfn "Array original:"
    printArray array

    let sortedArray = bubbleSort array
    printfn "Array ordenado:"
    printArray sortedArray

    0
```

Explicando o código:

O código começa definindo um módulo chamado `BubbleSort`. Dentro do módulo, há uma função chamada `bubbleSort` que recebe um array de inteiros e retorna o array ordenado usando o algoritmo Bubble Sort.

A função `bubbleSort` faz uso de duas variáveis mutáveis: `swapped` e `arr`. A variável `swapped` é usada para controlar o loop while e a variável `arr` é uma cópia do array original que será ordenado.

O loop while será executado até que não ocorram mais trocas de elementos no array. Ele começa definindo `swapped` como `true` e, em seguida, entra no loop while. 

Dentro do loop, há um loop for que percorre o array e compara cada elemento com o próximo. Se o elemento atual for maior que o próximo, eles são trocados de posição e a variável `swapped` é definida como `true`, indicando que houve uma troca.

Após a conclusão do loop while, o array ordenado é retornado pela função `bubbleSort`. 

Em seguida, há uma função `printArray` que é usada para imprimir o array na tela.

No ponto de entrada `main`, um array de exemplo é definido (`[| 64; 34; 25; 12; 22; 11; 90 |]`). Em seguida, o array original é impresso e a função `bubbleSort` é chamada para ordenar o array. O array ordenado é então impresso na tela.

Espero que este exemplo ilustre de forma clara como implementar um algoritmo de ordenação Bubble Sort em F#.