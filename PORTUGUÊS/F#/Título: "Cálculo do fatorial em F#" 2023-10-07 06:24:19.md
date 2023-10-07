Claro! Aqui está um exemplo de código em F# que realiza o cálculo do fatorial de um número usando programação funcional:

```fsharp
let rec factorial n =
    if n <= 1 then
        1
    else
        n * factorial (n - 1)

let input = System.Console.ReadLine()
let number = int input

let result = factorial number

printfn "O fatorial de %d é %d" number result
```

Neste código, definimos uma função chamada `factorial` que recebe um número inteiro `n` e retorna o fatorial desse número. A função é definida como recursiva, o que significa que ela chama a si mesma dentro do corpo da função.

A função `factorial` verifica se o número `n` é menor ou igual a 1. Se for, retorna 1 (um dos casos base da recursão). Caso contrário, calcula o fatorial multiplicando `n` pelo fatorial de `n - 1`.

Em seguida, utilizamos a função `System.Console.ReadLine()` para ler uma entrada do usuário. Essa entrada é convertida para um número inteiro usando `int input`.

Depois, chamamos a função `factorial` passando o número lido como argumento e armazenamos o resultado na variável `result`.

Por fim, utilizamos a função `printfn` para exibir o número original e seu fatorial calculado.

Esse código em F# é um exemplo de programação funcional, onde a ênfase é dada às funções puras e à imutabilidade dos dados. A recursão é uma técnica amplamente utilizada na programação funcional para resolver problemas complexos de forma elegante.