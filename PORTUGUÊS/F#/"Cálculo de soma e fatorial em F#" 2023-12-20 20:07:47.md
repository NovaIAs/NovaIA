Claro! Aqui está um exemplo de código complexo em F# que envolve manipulação de listas e recursão:

```fsharp
module Calculadora

open System

// Função para calcular a soma dos elementos de uma lista
let rec somarLista lista =
    match lista with
    | [] -> 0 // Caso base: lista vazia, retorna 0
    | hd :: tl -> hd + somarLista tl // Soma o primeiro elemento com a soma do restante da lista

// Função para calcular o fatorial de um número
let rec fatorial n =
    match n with
    | 0 -> 1 // Caso base: fatorial de 0 é 1
    | _ -> n * fatorial (n - 1) // Multiplica o número pelo fatorial do número anterior

// Função principal que executa o programa
let main argv =
    printfn "Bem-vindo(a) à Calculadora!"
    printfn "Digite uma opção:"
    printfn "1 - Somar elementos de uma lista"
    printfn "2 - Calcular o fatorial de um número"

    let opcao = Console.ReadLine()

    match opcao with
    | "1" ->
        printfn "Informe uma lista de números separados por espaço:"
        let numeros = Console.ReadLine().Split([|' '|], StringSplitOptions.RemoveEmptyEntries) |> Array.map int
        let resultado = somarLista numeros
        printfn "A soma dos elementos da lista é: %d" resultado
    | "2" ->
        printfn "Informe um número:"
        let numero = Console.ReadLine() |> int
        let resultado = fatorial numero
        printfn "O fatorial de %d é: %d" numero resultado
    | _ ->
        printfn "Opção inválida!"

    0 // Retorna 0 para indicar sucesso

[<EntryPoint>]
let main argv =
    main argv
```

Neste código, temos um módulo chamado "Calculadora" que contém duas funções: "somarLista" e "fatorial". A função "somarLista" recebe uma lista de números e utiliza a recursão para somar todos os elementos da lista. Já a função "fatorial" calcula o fatorial de um número utilizando a recursão também.

Na função principal "main", o programa exibe um menu de opções para o usuário escolher: somar os elementos de uma lista ou calcular o fatorial de um número. Dependendo da opção escolhida, o programa solicita os valores necessários e exibe o resultado correspondente.

O código está todo escrito em português e utiliza recursos da linguagem F# como casamento de padrões (pattern matching) e recursão.