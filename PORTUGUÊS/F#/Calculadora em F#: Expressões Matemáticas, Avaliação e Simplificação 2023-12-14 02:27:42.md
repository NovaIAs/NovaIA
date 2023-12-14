Claro! Aqui está um código complexo em F# que utiliza conceitos avançados de programação funcional e orientação a objetos:

```fsharp
module Calculadora

open System

// Define um tipo de dados abstrato para representar expressões matemáticas
type ExpressaoMath =
    | Constante of float
    | Variavel of string
    | Soma of ExpressaoMath * ExpressaoMath
    | Subtracao of ExpressaoMath * ExpressaoMath
    | Multiplicacao of ExpressaoMath * ExpressaoMath
    | Divisao of ExpressaoMath * ExpressaoMath
    | Seno of ExpressaoMath
    | Cosseno of ExpressaoMath
    | Tangente of ExpressaoMath

// Define uma função para avaliar uma expressão matemática
let rec avaliar expressao =
    match expressao with
    | Constante valor -> valor
    | Variavel _ -> failwith "Variável não pode ser avaliada diretamente"
    | Soma (e1, e2) -> avaliar e1 + avaliar e2
    | Subtracao (e1, e2) -> avaliar e1 - avaliar e2
    | Multiplicacao (e1, e2) -> avaliar e1 * avaliar e2
    | Divisao (e1, e2) -> avaliar e1 / avaliar e2
    | Seno e -> Math.Sin (avaliar e)
    | Cosseno e -> Math.Cos (avaliar e)
    | Tangente e -> Math.Tan (avaliar e)

// Define uma função para simplificar uma expressão matemática
let rec simplificar expressao =
    match expressao with
    | Constante _ -> expressao
    | Variavel _ -> expressao
    | Soma (e1, e2) ->
        let e1Simplificado = simplificar e1
        let e2Simplificado = simplificar e2
        match (e1Simplificado, e2Simplificado) with
        | Constante 0.0, _ -> e2Simplificado
        | _, Constante 0.0 -> e1Simplificado
        | Constante v1, Constante v2 -> Constante (v1 + v2)
        | _ -> Soma (e1Simplificado, e2Simplificado)
    | Subtracao (e1, e2) ->
        let e1Simplificado = simplificar e1
        let e2Simplificado = simplificar e2
        match (e1Simplificado, e2Simplificado) with
        | Constante 0.0, _ -> Negacao e2Simplificado
        | _, Constante 0.0 -> e1Simplificado
        | Constante v1, Constante v2 -> Constante (v1 - v2)
        | _ -> Subtracao (e1Simplificado, e2Simplificado)
    | Multiplicacao (e1, e2) ->
        let e1Simplificado = simplificar e1
        let e2Simplificado = simplificar e2
        match (e1Simplificado, e2Simplificado) with
        | Constante 0.0, _ -> Constante 0.0
        | _, Constante 0.0 -> Constante 0.0
        | Constante 1.0, _ -> e2Simplificado
        | _, Constante 1.0 -> e1Simplificado
        | Constante v1, Constante v2 -> Constante (v1 * v2)
        | _ -> Multiplicacao (e1Simplificado, e2Simplificado)
    | Divisao (e1, e2) ->
        let e1Simplificado = simplificar e1
        let e2Simplificado = simplificar e2
        match (e1Simplificado, e2Simplificado) with
        | Constante 0.0, _ -> Constante 0.0
        | _, Constante 1.0 -> e1Simplificado
        | Constante v1, Constante v2 -> Constante (v1 / v2)
        | _ -> Divisao (e1Simplificado, e2Simplificado)
    | Seno e ->
        let eSimplificado = simplificar e
        match eSimplificado with
        | Constante v -> Constante (Math.Sin v)
        | _ -> Seno eSimplificado
    | Cosseno e ->
        let eSimplificado = simplificar e
        match eSimplificado with
        | Constante v -> Constante (Math.Cos v)
        | _ -> Cosseno eSimplificado
    | Tangente e ->
        let eSimplificado = simplificar e
        match eSimplificado with
        | Constante v -> Constante (Math.Tan v)
        | _ -> Tangente eSimplificado

// Exemplo de uso da calculadora
let expressao = Soma (Constante 2.0, Multiplicacao (Constante 3.0, Constante 4.0))
printfn "Expressão: %A" expressao
let resultado = avaliar expressao
printfn "Resultado: %f" resultado
let expressaoSimplificada = simplificar expressao
printfn "Expressão simplificada: %A" expressaoSimplificada
```

Neste código, implementei uma calculadora que permite definir expressões matemáticas complexas usando tipos de dados abstratos. A calculadora suporta constantes, variáveis, operações básicas (soma, subtração, multiplicação, divisão) e funções trigonométricas (seno, cosseno, tangente).

A função `avaliar` é responsável por avaliar uma expressão matemática, retornando o resultado numérico. A função `simplificar` simplifica a expressão, aplicando regras de simplificação matemática sempre que possível.

No exemplo de uso, criei uma expressão `2 + (3 * 4)` e avaliei o resultado, que foi impresso na tela. Em seguida, simplifiquei a expressão e também a imprimo na tela.