Aqui está um exemplo de um código em F# que cria uma árvore binária e implementa a função de busca binária:

```fsharp
open System

// Definindo um tipo de dado para a árvore binária
type ArvoreBinaria<'T> =
    | NoNulo
    | No of 'T * ArvoreBinaria<'T> * ArvoreBinaria<'T>

// Função para inserir um elemento na árvore binária
let rec inserirElemento elemento arvore =
    match arvore with
    | NoNulo -> No(elemento, NoNulo, NoNulo)
    | No(valor, esquerda, direita) ->
        if elemento < valor then
            No(valor, inserirElemento elemento esquerda, direita)
        else
            No(valor, esquerda, inserirElemento elemento direita)

// Função para verificar se um elemento está presente na árvore binária
let rec buscarElemento elemento arvore =
    match arvore with
    | NoNulo -> false
    | No(valor, esquerda, direita) ->
        if elemento = valor then
            true
        elif elemento < valor then
            buscarElemento elemento esquerda
        else
            buscarElemento elemento direita

// Criação da árvore binária e busca por elementos
let arvore =
    let elementos = [5; 3; 8; 2; 4; 7; 9]
    let rec inserirElementos arvore elementos =
        match elementos with
        | [] -> arvore
        | hd::tl -> inserirElementos (inserirElemento hd arvore) tl
    inserirElementos NoNulo elementos

let elementosBusca = [4; 7; 10]
let resultadoBusca =
    elementosBusca
    |> List.map (fun elemento -> buscarElemento elemento arvore)

printfn "Elementos buscados: %A" elementosBusca
printfn "Resultado da busca: %A" resultadoBusca
```

Neste código, temos a definição de um tipo de dado chamado `ArvoreBinaria`, que representa um nó em uma árvore binária. Em seguida, temos a função `inserirElemento`, que insere um elemento em uma árvore binária de forma ordenada. A função `buscarElemento` verifica se um elemento está presente na árvore binária.

Em seguida, criamos uma árvore binária com alguns elementos utilizando a função `inserirElementos`. Por fim, realizamos a busca por elementos específicos na árvore utilizando a função `buscarElemento`, e exibimos os resultados.

Esse código é um exemplo simples de como trabalhar com árvores binárias e busca binária em F#. É importante notar que existem muitas outras maneiras de implementar esse tipo de estrutura de dados e algoritmo, dependendo das necessidades específicas do projeto.