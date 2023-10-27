Claro! Aqui está um código complexo em F# que envolve o conceito de programação funcional e usa uma abordagem recursiva para encontrar o n-ésimo número da sequência de Fibonacci:

```fsharp
// Função recursiva para calcular o n-ésimo número da sequência de Fibonacci
let rec fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

// Função para receber a entrada do usuário e imprimir o resultado
let main() =
    printfn "Digite o valor de n para calcular o n-ésimo número da sequência de Fibonacci:"
    let input = Console.ReadLine()
    let n = int input

    printfn "O %d-ésimo número da sequência de Fibonacci é: %d" n (fibonacci n)

// Chamada da função principal
main()
```

Este código começa definindo uma função `fibonacci` que recebe um número inteiro `n` como entrada e retorna o n-ésimo número da sequência de Fibonacci. 

A função `fibonacci` é implementada usando correspondência de padrões (`match`). Se `n` for 0, retorna 0; se `n` for 1, retorna 1; caso contrário, chama recursivamente a função `fibonacci` com `n-1` e `n-2` e soma os resultados.

Em seguida, a função `main` solicita ao usuário que digite o valor de `n`, lê a entrada do usuário e converte-a para um número inteiro. Em seguida, imprime o resultado chamando a função `fibonacci` com o valor de `n`.

Por fim, a função `main` é chamada para iniciar a execução do programa.

Espero que este exemplo complexo atenda às suas expectativas!