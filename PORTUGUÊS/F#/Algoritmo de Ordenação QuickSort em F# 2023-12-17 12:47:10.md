Claro! Aqui está um exemplo de um código em F# que implementa um algoritmo de ordenação chamado QuickSort:

```fsharp
module QuickSort

let rec quickSort (lista: int list) : int list =
    match lista with
    | [] -> []
    | [x] -> [x]
    | pivot :: tail ->
        let lesser = List.filter (fun x -> x <= pivot) tail
        let greater = List.filter (fun x -> x > pivot) tail
        let sortedLesser = quickSort lesser
        let sortedGreater = quickSort greater
        sortedLesser @ [pivot] @ sortedGreater

let listaDesordenada = [4; 2; 7; 1; 9; 3; 6; 5; 8]
let listaOrdenada = quickSort listaDesordenada

printfn "Lista desordenada: %A" listaDesordenada
printfn "Lista ordenada: %A" listaOrdenada
```

Explicação do código:

1. Definimos um módulo chamado "QuickSort" usando a palavra-chave `module`.

2. Dentro do módulo, definimos uma função chamada `quickSort` que recebe uma lista de inteiros e retorna uma lista de inteiros. A função é recursiva.

3. A função `quickSort` utiliza a construção `match` para lidar com diferentes casos da lista:

   - Se a lista estiver vazia (`[]`), retornamos uma lista vazia também.
   - Se tiver apenas um elemento (`[x]`), retornamos a lista inalterada.
   - Caso contrário, temos pelo menos dois elementos na lista, então escolhemos um elemento chamado "pivô" (neste caso, o primeiro elemento `pivot :: tail`), e dividimos a lista em duas partes: uma contendo os elementos menores ou iguais ao pivô (`lesser`), e outra contendo os elementos maiores que o pivô (`greater`).
   
4. Chamamos recursivamente a função `quickSort` nas duas partes da lista (`sortedLesser` e `sortedGreater`) para ordená-las.

5. Por fim, concatenamos as três partes (`sortedLesser`, `[pivot]` e `sortedGreater`) usando o operador `@` para obter a lista final ordenada.

6. Em seguida, definimos uma lista de exemplo chamada `listaDesordenada`.

7. Chamamos a função `quickSort` passando `listaDesordenada` como argumento e atribuímos o resultado à variável `listaOrdenada`.

8. Usamos a função `printfn` para exibir as duas listas na saída do console.

Espero que isso seja útil para você! Se tiver alguma dúvida, é só perguntar.