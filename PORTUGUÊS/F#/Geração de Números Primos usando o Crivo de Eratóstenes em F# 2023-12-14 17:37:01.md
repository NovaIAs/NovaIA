Claro! Aqui está um código em F# que gerará uma sequência de números primos usando o Crivo de Eratóstenes. O Crivo de Eratóstenes é um algoritmo eficiente para encontrar todos os números primos até um determinado limite. 

```fsharp
open System

let crivoEratostenes limite =
    let numeros = Array.create (limite+1) true
    let mutable i = 2
    while i*i <= limite do
        if numeros.[i] then
            let mutable j = i*i
            while j <= limite do
                numeros.[j] <- false
                j <- j + i
        i <- i + 1
    let primos = [2..limite] |> List.filter (fun x -> numeros.[x])
    primos

[<EntryPoint>]
let main argv =
    printfn "Digite o limite para a geração dos números primos:"
    let limite = Console.ReadLine() |> int
    let primos = crivoEratostenes limite
    printfn "Números primos menores ou iguais a %d:" limite
    primos |> List.iter (printf "%d ")
    0 // Retorna 0 para indicar que o programa foi executado com sucesso
```

Neste código, a função `crivoEratostenes` recebe um limite como parâmetro e retorna uma lista de todos os números primos menores ou iguais a esse limite. O algoritmo utiliza uma matriz booleana para marcar os números que são compostos (não primos). 

Dentro do laço principal, começamos com o número 2 (o menor número primo) e percorremos todos os múltiplos desse número até o limite fornecido. Para cada múltiplo encontrado, marcamos o número correspondente na matriz como falso. 

Após o término do laço, filtramos os números que ainda estão marcados como true na matriz e os adicionamos à lista de primos. Por fim, imprimimos essa lista na tela.

No método `main`, solicitamos ao usuário que digite o limite desejado e chamamos a função `crivoEratostenes` com esse limite. Em seguida, imprimimos os números primos encontrados na tela.

Espero que esse código complexo atenda às suas expectativas! Se você precisar de mais ajuda, é só me perguntar.