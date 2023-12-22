Claro! Aqui está um exemplo de código F# complexo e diferenciado:

```fsharp
module Calculadora

open System

type Operacao = Soma of int * int
              | Subtracao of int * int
              | Multiplicacao of int * int
              | Divisao of int * int
              | Potencia of int * int

let realizarOperacao (op: Operacao) =
    match op with
    | Soma (x, y) -> x + y
    | Subtracao (x, y) -> x - y
    | Multiplicacao (x, y) -> x * y
    | Divisao (x, y) -> x / y
    | Potencia (x, y) -> int32(Math.Pow(float32(x), float32(y)))

let main args =
    printfn "Bem-vindo à Calculadora!"
    printfn "Digite o primeiro número: "
    let mutable numero1 = Console.ReadLine()
    printfn "Digite o segundo número: "
    let mutable numero2 = Console.ReadLine()

    let mutable operacaoEscolhida = ""
    while operacaoEscolhida <> "sair" do
        printfn "Escolha a operação: (soma, subtracao, multiplicacao, divisao, potencia)"
        operacaoEscolhida <- Console.ReadLine().ToLower()

        match operacaoEscolhida with
        | "soma" ->
            let resultado = realizarOperacao (Soma (Int32.Parse(numero1), Int32.Parse(numero2)))
            printfn "Resultado: %d" resultado
        | "subtracao" ->
            let resultado = realizarOperacao (Subtracao (Int32.Parse(numero1), Int32.Parse(numero2)))
            printfn "Resultado: %d" resultado
        | "multiplicacao" ->
            let resultado = realizarOperacao (Multiplicacao (Int32.Parse(numero1), Int32.Parse(numero2)))
            printfn "Resultado: %d" resultado
        | "divisao" ->
            let resultado = realizarOperacao (Divisao (Int32.Parse(numero1), Int32.Parse(numero2)))
            printfn "Resultado: %d" resultado
        | "potencia" ->
            let resultado = realizarOperacao (Potencia (Int32.Parse(numero1), Int32.Parse(numero2)))
            printfn "Resultado: %d" resultado
        | _ ->
            printfn "Operação inválida!"

        printfn "Digite o primeiro número novamente (ou 'sair' para encerrar): "
        numero1 <- Console.ReadLine()
        printfn "Digite o segundo número novamente (ou 'sair' para encerrar): "
        numero2 <- Console.ReadLine()

    0 // Retorna 0 para indicar que o programa foi executado com sucesso
```

Neste código, implementei uma calculadora em F# que permite ao usuário realizar operações matemáticas simples, como soma, subtração, multiplicação, divisão e potência. O programa utiliza tipos de discriminação de união (union types) para representar as diferentes operações.

No início do programa, o usuário é solicitado a inserir dois números. Em seguida, ele pode escolher qual operação deseja executar. O programa faz a correspondência da escolha do usuário com a operação correspondente e chama a função `realizarOperacao` para executar a operação selecionada.

A função `realizarOperacao` utiliza correspondência de padrões (pattern matching) para calcular o resultado com base no tipo de operação recebida como argumento.

O programa continua solicitando ao usuário que insira novos números até que a palavra "sair" seja digitada.

Espero que este código atenda às suas expectativas!