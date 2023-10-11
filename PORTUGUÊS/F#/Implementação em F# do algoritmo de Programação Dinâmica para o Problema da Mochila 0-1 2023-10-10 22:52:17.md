Claro! Aqui está um exemplo de código em F# que usa uma técnica chamada Programação Dinâmica para resolver o problema da Mochila 0-1.

```fsharp
open System

let knapsack weights values capacity =
    let n = weights.Length
    let dp = Array2D.create (n + 1) (capacity + 1) 0

    for i = 1 to n do
        for j = 1 to capacity do
            if weights.[i - 1] > j then
                dp.[i, j] <- dp.[i - 1, j]
            else
                dp.[i, j] <- max (dp.[i - 1, j]) (values.[i - 1] + dp.[i - 1, j - weights.[i - 1]])

    let mutable i = n
    let mutable j = capacity

    let mutable selectedItems = []
    while i > 0 && j > 0 do
        if dp.[i, j] <> dp.[i - 1, j] then
            selectedItems <- i :: selectedItems
            j <- j - weights.[i - 1]
        i <- i - 1

    selectedItems

// Exemplo de uso
let weights = [|2; 3; 4; 5|]
let values = [|3; 4; 5; 6|]
let capacity = 7

let selectedItems = knapsack weights values capacity

printfn "Itens selecionados:"
for item in selectedItems do
    printfn "Item %d: Peso = %d, Valor = %d" item weights.[item - 1] values.[item - 1]
```

Este código implementa o algoritmo da Mochila 0-1, que é usado para resolver problemas de otimização em que se deseja maximizar o valor total de itens selecionados, dadas restrições de capacidade de uma mochila.

O algoritmo utiliza uma tabela de programação dinâmica representada pela matriz `dp`. Cada célula `dp[i, j]` representa o valor máximo que pode ser alcançado usando os primeiros `i` itens e uma capacidade de mochila `j`. O valor nessa célula é calculado com base nas soluções anteriores.

A parte principal do código é o loop duplo que percorre os itens e as capacidades. Se o peso do item `i` for maior que a capacidade `j`, significa que o item não pode ser incluído na mochila, então o valor máximo é simplesmente o valor máximo obtido sem esse item, ou seja, `dp.[i - 1, j]`. Caso contrário, o código compara o valor máximo obtido sem o item `i` (`dp.[i - 1, j]`) com o valor máximo obtido incluindo o item `i` (`values.[i - 1] + dp.[i - 1, j - weights.[i - 1]]`) e escolhe o maior valor como o valor máximo para a célula atual.

Depois de calcular a tabela `dp`, o código faz um rastreamento inverso para determinar quais itens foram selecionados. Isso é feito verificando se a célula atual `dp.[i, j]` é diferente da célula acima `dp.[i - 1, j]`. Se for diferente, significa que o item `i` foi selecionado, então ele é adicionado à lista `selectedItems` e a capacidade é atualizada subtraindo o peso do item.

Por fim, o código imprime os itens selecionados juntamente com seus pesos e valores correspondentes.

Espero que este código ilustre a aplicação da Programação Dinâmica na resolução de problemas de otimização, como o problema da Mochila 0-1.