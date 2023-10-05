Claro! Segue abaixo um exemplo de código complexo em F#, com uma breve explicação de seu funcionamento:

```fsharp
open System

// Função para verificar se um número é primo
let isPrime (num:int) : bool =
    if num < 2 then
        false
    else
        let limit = int32(Math.Sqrt(float num))
        let rec checkDivisor (divisor:int) : bool =
            if divisor > limit then
                true
            else if num % divisor = 0 then
                false
            else
                checkDivisor (divisor + 1)
        checkDivisor 2

// Função para imprimir os números primos até um determinado limite
let printPrimes (limit:int) : unit =
    printfn "Números primos até %d:" limit
    for i in 2..limit do
        if isPrime i then
            printf "%d " i
    printfn ""

// Função principal
[<EntryPoint>]
let main argv =
    let limit = 100
    printPrimes limit
    0 // Código de retorno

```

Explicação do código:

1. Abrimos o namespace `System` para que possamos utilizar a função `Math.Sqrt`.
2. Definimos a função `isPrime` que recebe um número inteiro e retorna um valor booleano indicando se o número é primo ou não. A função primeiro verifica se o número é menor que 2, caso seja, retorna `false`. Em seguida, calculamos a raiz quadrada do número e definimos uma função recursiva interna chamada `checkDivisor` que verifica se o número é divisível por algum número entre 2 e a raiz quadrada do próprio número. Se for divisível por algum número, retorna `false`, caso contrário, retorna `true`.
3. Definimos a função `printPrimes` que recebe um limite inteiro e imprime todos os números primos até esse limite. A função itera de 2 até o limite e, para cada número, verifica se é primo utilizando a função `isPrime`. Se for primo, imprime o número.
4. Por fim, definimos a função principal `[<EntryPoint>]` que será o ponto de entrada do programa. Neste exemplo, definimos um limite de 100 e chamamos a função `printPrimes` passando esse limite. Em seguida, retornamos 0 para indicar que o programa foi executado com sucesso.

Espero que esse código complexo em F# atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.