Claro! Aqui está um código complexo em F#, que faz uso de recursão e expressões lambda para resolver um problema matemático conhecido como "Triângulo de Pascal":

```fsharp
open System

let rec calculaLinhaPascal linha =
    match linha with
    | 0 -> [1]
    | _ ->
        let linhaAnterior = calculaLinhaPascal (linha - 1)
        let novoElemento = 
            linhaAnterior
            |> Seq.append (Seq.skip 1 linhaAnterior)
            |> Seq.pairwise
            |> Seq.map (fun (a, b) -> a + b)
        Seq.append [1] (Seq.append novoElemento [1])

let rec calculaTrianguloPascal n =
    match n with
    | 0 -> [[1]]
    | _ ->
        let trianguloAnterior = calculaTrianguloPascal (n - 1)
        trianguloAnterior @ [calculaLinhaPascal n]

let imprimirTrianguloPascal n =
    let triangulo = calculaTrianguloPascal n
    triangulo
    |> List.iter (fun linha -> printfn "%A" linha)

[<EntryPoint>]
let main argv =
    printfn "Digite o número de linhas do Triângulo de Pascal:"
    let n = Console.ReadLine() |> int
    imprimirTrianguloPascal n
    0
```

Neste código, temos a definição de duas funções recursivas: `calculaLinhaPascal` e `calculaTrianguloPascal`. 

A função `calculaLinhaPascal` recebe como parâmetro um número de linha do Triângulo de Pascal e retorna uma lista contendo os elementos dessa linha. Ela utiliza pattern matching para tratar os casos base (linha 0) e recursivamente chama a si mesma para calcular os elementos das linhas subsequentes. A lógica para calcular os elementos de cada linha é baseada na propriedade de que cada elemento é a soma dos dois elementos imediatamente acima na linha anterior.

A função `calculaTrianguloPascal` recebe como parâmetro um número `n` e retorna uma lista de listas representando as `n` primeiras linhas do Triângulo de Pascal. Ela faz uso da função `calculaLinhaPascal` para calcular cada linha, chamando-se recursivamente para obter as linhas anteriores.

Por fim, temos a função `imprimirTrianguloPascal` que recebe um número `n` e imprime o Triângulo de Pascal com `n` linhas. Ela utiliza a função `calculaTrianguloPascal` para obter o Triângulo de Pascal e, em seguida, itera sobre cada linha e imprime-a utilizando a função `printfn`.

No ponto de entrada `main`, o programa solicita ao usuário o número de linhas desejado, lê o valor digitado, chama a função `imprimirTrianguloPascal` passando esse valor e encerra a execução retornando 0.

Espero que esse código atenda às suas expectativas!