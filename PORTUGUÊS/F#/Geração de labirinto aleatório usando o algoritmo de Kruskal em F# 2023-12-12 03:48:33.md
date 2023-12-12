Claro! Aqui está um código complexo em F# que gera um labirinto aleatório usando o algoritmo de Kruskal:

```fsharp
open System

type Cell = {
    mutable set: int
    mutable walls: bool array
}

let generateMaze (width: int) (height: int) =
    let rng = new Random()
    let cells = Array2D.create width height { set = 0; walls = Array.create 4 true }
    let allWalls = Array.create ((width - 1) * height + (height - 1) * width) (0, 0)

    let rec find (i: int) =
        if i <> cells.[i].set then
            cells.[i].set <- find cells.[i].set
        cells.[i].set

    let rec union (i: int) (j: int) =
        let ri = find i
        let rj = find j
        if ri <> rj then
            cells.[ri].set <- rj

    let shuffle (arr: 'a array) =
        let n = arr.Length
        for i = n - 1 downto 1 do
            let j = rng.Next(i + 1)
            let temp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- temp

    let rec carvePassages (x: int) (y: int) =
        let directions = [| (0, 1); (0, -1); (1, 0); (-1, 0) |]
        shuffle directions

        for d in directions do
            let nx = x + fst d
            let ny = y + snd d
            if nx >= 0 && nx < width && ny >= 0 && ny < height && cells.[nx, ny].set <> cells.[x, y].set then
                cells.[x, y].walls.[Array.findIndex ((=) (x, y)) allWalls] <- false
                cells.[nx, ny].walls.[Array.findIndex ((=) (nx, ny)) allWalls] <- false
                union (x + y * width) (nx + ny * width)
                carvePassages nx ny

    let rec printMaze (x: int) (y: int) =
        let cell = cells.[x, y]
        printf "X"
        if cell.walls.[0] then printf "-"
        else printf " "
        printf "X"
        printfn ""
        if cell.walls.[3] then printf "|"
        else printf " "
        if y = height - 1 then printf "X"
        else printf " "
        if x = width - 1 then printf "X"
        else if cell.walls.[2] then printf "-"
        else printf " "
        if x = width - 1 && y = height - 1 then printf "X"
        else printMaze (x + 1) (y + 1)

    for i = 0 to width - 1 do
        for j = 0 to height - 1 do
            if i < width - 1 then
                allWalls.[i + j * (width - 1)] <- (i, j)
            if j < height - 1 then
                allWalls.[(width - 1) * height + i + j * width] <- (i, j)

    shuffle allWalls

    for i = 0 to width - 1 do
        cells.[i, 0].set <- i
    for i = 0 to height - 1 do
        cells.[0, i].set <- i * width

    carvePassages 0 0
    printMaze 0 0
```

O código acima utiliza a biblioteca padrão do F# e começa definindo o tipo `Cell`, que representa uma célula no labirinto. A função `generateMaze` é responsável por gerar o labirinto, recebendo a largura e altura desejadas como parâmetros.

A função `find` implementa a operação find da estrutura de dados Disjoint Set (união-busca), que é utilizada para determinar se duas células pertencem ao mesmo conjunto. A função `union` realiza a operação union, que combina dois conjuntos distintos em um único conjunto.

A função `shuffle` é responsável por embaralhar aleatoriamente um array. Em seguida, a função `carvePassages` utiliza o algoritmo de Kruskal para criar caminhos no labirinto, removendo paredes entre células não conectadas.

Por fim, a função `printMaze` imprime o labirinto na tela, utilizando caracteres ASCII para representar as paredes e caminhos.

Para executar o código acima, basta chamar a função `generateMaze` com a largura e altura desejadas:

```fsharp
generateMaze 10 10
```

Isso irá gerar um labirinto aleatório com largura 10 e altura 10 e exibi-lo no console.