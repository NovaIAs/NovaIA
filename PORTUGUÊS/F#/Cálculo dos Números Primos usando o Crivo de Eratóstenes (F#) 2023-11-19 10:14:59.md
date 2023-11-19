Claro! Aqui está um exemplo de um código em F# que calcula os números primos usando uma abordagem de crivo de Eratóstenes:

```fsharp
open System

let calcularPrimos limite =
    let numeros = Array.create (limite + 1) true
    numeros.[0] <- false
    numeros.[1] <- false

    for i = 2 to int (sqrt (float limite)) do
        if numeros.[i] then
            let mutable j = i * i
            while j <= limite do
                numeros.[j] <- false
                j <- j + i

    let primos = numeros
                |> Array.mapi (fun i isPrimo -> if isPrimo then Some i else None)
                |> Array.choose id

    primos

let limite = 100
let primos = calcularPrimos limite

printfn "Números primos até %d:" limite
for primo in primos do
    printf "%d " primo
```

Explicação do código:

1. A linha `open System` importa o namespace `System`, onde está a função `sqrt` usada posteriormente.
2. A função `calcularPrimos` recebe um limite e retorna uma lista de números primos até esse limite.
3. Primeiro, um array chamado `numeros` é criado com o tamanho do limite e todos os elementos são definidos como verdadeiros.
4. Os números 0 e 1 são marcados como falsos, pois não são primos.
5. A partir do número 2 (primeiro número primo), percorremos os números até a raiz quadrada do limite.
6. Se o número atual for verdadeiro, significa que é primo, então atualizamos seus múltiplos como falsos.
7. Usamos o laço `for` para percorrer os múltiplos começando pelo quadrado do número atual.
8. Por exemplo, para o número 2, atualizamos como falsos os múltiplos 4, 6, 8, etc.
9. Depois de marcarmos todos os múltiplos, a função continua para o próximo número primo.
10. No final, usamos o `Array.mapi` para mapear o array de números primos para uma nova array, onde os elementos primos são colocados na posição correta e os elementos não primos são descartados.
11. A função `Array.choose` filtra os valores `Some` (primos) e descarta os valores `None`.
12. Finalmente, a lista de números primos é impressa na tela.