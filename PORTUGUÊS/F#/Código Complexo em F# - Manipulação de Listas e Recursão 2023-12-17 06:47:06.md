Claro! Aqui está um exemplo de um código complexo em F# que envolve manipulação de listas e recursão:

```fsharp
// Função para calcular a soma dos elementos em uma lista de inteiros
let rec somaLista (lista: int list) =
    match lista with
    | [] -> 0
    | head :: tail -> head + somaLista tail

// Função para encontrar o máximo elemento em uma lista de inteiros
let rec maximoLista (lista: int list) =
    match lista with
    | [] -> failwith "Lista vazia!"
    | [x] -> x
    | head :: tail -> max head (maximoLista tail)

// Função para ordenar uma lista de inteiros em ordem crescente
let rec ordenarLista (lista: int list) =
    match lista with
    | [] -> []
    | [x] -> [x]
    | _ ->
        let pivo = List.head lista
        let menores = List.filter (fun x -> x < pivo) lista
        let iguais = List.filter (fun x -> x = pivo) lista
        let maiores = List.filter (fun x -> x > pivo) lista
        (ordenarLista menores) @ iguais @ (ordenarLista maiores)

// Exemplo de uso das funções acima
let listaExemplo = [9; 5; 2; 7; 1; 10; 4; 8; 3; 6]
let soma = somaLista listaExemplo
let maximo = maximoLista listaExemplo
let listaOrdenada = ordenarLista listaExemplo

printfn "Lista: %A" listaExemplo
printfn "Soma: %d" soma
printfn "Máximo: %d" maximo
printfn "Lista Ordenada: %A" listaOrdenada
```

Neste código, temos três funções principais: `somaLista`, `maximoLista` e `ordenarLista`. A função `somaLista` calcula a soma de todos os elementos em uma lista de inteiros através de recursão. A função `maximoLista` encontra o maior elemento em uma lista de inteiros usando a função `max` do F#. A função `ordenarLista` ordena uma lista de inteiros em ordem crescente utilizando o algoritmo quicksort.

No exemplo de uso no final do código, criamos uma lista de exemplo `listaExemplo` e aplicamos as três funções para calcular a soma dos elementos, encontrar o máximo e ordenar a lista.

Ao executar o código, você verá a lista original, o resultado da soma, o máximo e a lista ordenada sendo exibidos no console.

Espero que isso atenda ao seu pedido por um código complexo e diferenciado em F#! Se você tiver mais alguma solicitação, é só me avisar!