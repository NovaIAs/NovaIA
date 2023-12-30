Claro! Aqui está um exemplo de um código complexo em F# que envolve manipulação de listas e recursão:

```fsharp
// Função que retorna a soma de todos os elementos de uma lista de números
let rec somaLista (lista: int list) : int =
    match lista with
    | [] -> 0 // Caso base: lista vazia, retorna 0
    | head::tail -> head + somaLista tail // Soma o primeiro elemento com a soma dos demais

// Função que retorna a média de todos os elementos de uma lista de números
let mediaLista (lista: int list) : float =
    let soma = float (somaLista lista)
    let tamanho = float (List.length lista)
    soma / tamanho

// Função que retorna o maior elemento de uma lista de números
let rec maiorElemento (lista: int list) : int =
    match lista with
    | [] -> invalidArg "lista" "A lista não pode ser vazia!" // Caso base: lista vazia, gera uma exceção
    | [x] -> x // Caso base: lista com apenas um elemento, retorna o próprio elemento
    | head::tail -> max head (maiorElemento tail) // Compara o primeiro elemento com o maior dos demais

// Função que retorna uma lista com todos os elementos maiores que um valor específico
let elementosMaioresQue (valor: int) (lista: int list) : int list =
    List.filter (fun x -> x > valor) lista

// Função principal
let main () =
    let numeros = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
    printfn "Lista original: %A" numeros
    
    let soma = somaLista numeros
    printfn "Soma: %d" soma
    
    let media = mediaLista numeros
    printfn "Média: %f" media
    
    let maior = maiorElemento numeros
    printfn "Maior elemento: %d" maior
    
    let maioresQue5 = elementosMaioresQue 5 numeros
    printfn "Elementos maiores que 5: %A" maioresQue5

// Chamada da função principal
main ()
```

Neste código, temos a definição de várias funções. A função `somaLista` utiliza recursão para somar todos os elementos de uma lista. A função `mediaLista` utiliza a função `somaLista` para calcular a média dos elementos de uma lista.

A função `maiorElemento` utiliza recursão para encontrar o maior elemento de uma lista. Caso a lista seja vazia, a função gera uma exceção.

Por fim, a função `elementosMaioresQue` utiliza a função `List.filter` para filtrar todos os elementos de uma lista que são maiores que um valor específico.

No bloco principal `main`, são criados alguns números de exemplo e as funções são chamadas para demonstrar seu funcionamento.