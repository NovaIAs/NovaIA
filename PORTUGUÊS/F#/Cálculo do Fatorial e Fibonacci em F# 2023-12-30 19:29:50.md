Claro! Aqui está um código complexo em F# que utiliza recursão para calcular o fatorial de um número:

```fsharp
let rec fatorial n =
    match n with
    | 0 -> 1
    | _ -> n * fatorial (n - 1)

let rec fibonacci n =
    match n with
    | 0 -> 0
    | 1 -> 1
    | _ -> fibonacci (n - 1) + fibonacci (n - 2)

let rec imprimirFatorialFibonacci n =
    if n >= 0 then
        let f = fatorial n
        let fib = fibonacci n
        printfn "O fatorial de %d é: %d" n f
        printfn "O %dº número de Fibonacci é: %d" n fib
        imprimirFatorialFibonacci (n - 1)

imprimirFatorialFibonacci 10
```

Neste código, temos três funções principais: `fatorial`, `fibonacci` e `imprimirFatorialFibonacci`. 

A função `fatorial` é uma função recursiva que recebe um número `n` e retorna o fatorial desse número. Ela utiliza o padrão `match` para verificar se `n` é igual a zero, caso seja, retorna 1, caso contrário, realiza a multiplicação de `n` pelo fatorial de `n - 1`.

A função `fibonacci` também é uma função recursiva. Ela recebe um número `n` e retorna o `n`-ésimo número na sequência de Fibonacci. Novamente, a função utiliza o padrão `match` para verificar se `n` é igual a zero ou a um, e retorna o valor correspondente. Caso contrário, adiciona os resultados dos números anteriores na sequência.

A função `imprimirFatorialFibonacci` é responsável por imprimir o fatorial e o número de Fibonacci para cada número de 10 até 0, chamando recursivamente a si mesma até que `n` seja menor do que zero. Ela utiliza as funções `fatorial` e `fibonacci` para calcular os valores e os imprime usando `printfn`.

Ao executar este código, você verá a saída mostrando o fatorial e o número de Fibonacci para cada número de 10 até 0.