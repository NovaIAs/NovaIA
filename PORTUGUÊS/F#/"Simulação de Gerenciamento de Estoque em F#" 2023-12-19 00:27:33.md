Claro! Aqui está um código complexo em F# que realiza uma simulação de um sistema de gerenciamento de estoque de uma loja virtual.

```fsharp
open System

type Produto = {
    Nome: string
    Preco: float
    Quantidade: int
}

type Estoque = {
    Produtos: Produto list
    AtualizarEstoque: string -> int -> Estoque
    VerificarDisponibilidade: string -> bool
    CalcularValorTotal: float
    OrdenarProdutosPorPreco: unit -> Produto list
    ImprimirRelatorio: unit -> unit
}

let estoqueInicial = {
    Produtos = [
        { Nome = "Camiseta", Preco = 29.99, Quantidade = 50 }
        { Nome = "Calça Jeans", Preco = 79.99, Quantidade = 20 }
        { Nome = "Tênis", Preco = 99.99, Quantidade = 30 }
    ]
    AtualizarEstoque = fun nome qtd ->
        { Produtos = List.map (fun p ->
            if p.Nome = nome then { p with Quantidade = p.Quantidade + qtd }
            else p) estoqueInicial.Produtos }
    VerificarDisponibilidade = fun nome ->
        List.exists (fun p -> p.Nome = nome && p.Quantidade > 0) estoqueInicial.Produtos
    CalcularValorTotal = 
        List.sumBy (fun p -> p.Preco * float p.Quantidade) estoqueInicial.Produtos
    OrdenarProdutosPorPreco = fun () ->
        List.sortBy (fun p -> p.Preco) estoqueInicial.Produtos
    ImprimirRelatorio = fun () ->
        printfn "Relatório de estoque:"

        let produtosOrdenados = estoqueInicial.OrdenarProdutosPorPreco ()
        List.iter (fun p ->
            printfn "Nome: %s, Preço: R$%.2f, Quantidade: %d" p.Nome p.Preco p.Quantidade) produtosOrdenados

        printfn "Valor total do estoque: R$%.2f" estoqueInicial.CalcularValorTotal
}

let novoEstoque = estoqueInicial.AtualizarEstoque "Camiseta" 10
printfn "Quantidade de camisetas no estoque: %d" (List.find (fun p -> p.Nome = "Camiseta") novoEstoque.Produtos).Quantidade

let disponibilidade = estoqueInicial.VerificarDisponibilidade "Tênis"
if disponibilidade then
    printfn "O produto está disponível no estoque."
else
    printfn "O produto não está disponível no estoque."

estoqueInicial.ImprimirRelatorio ()
```

Neste código, criamos dois tipos, `Produto` e `Estoque`. O tipo `Produto` representa um produto específico em nosso estoque, com propriedades como nome, preço e quantidade. O tipo `Estoque` é uma estrutura que mantém uma lista de produtos e possui diversas funções para gerenciar o estoque.

A função `AtualizarEstoque` permite atualizar a quantidade de um produto no estoque, dado seu nome e a quantidade a ser adicionada ou removida. A função `VerificarDisponibilidade` verifica se um produto está disponível no estoque com base em seu nome. A função `CalcularValorTotal` calcula o valor total do estoque, somando o preço de cada produto multiplicado pela quantidade disponível. A função `OrdenarProdutosPorPreco` retorna a lista de produtos ordenados por preço crescente. A função `ImprimirRelatorio` imprime um relatório completo do estoque, listando os produtos ordenados por preço e exibindo o valor total do estoque.

No exemplo final, demonstramos como usar essas funcionalidades. Primeiro, atualizamos a quantidade de camisetas adicionando 10 unidades. Em seguida, verificamos a disponibilidade de tênis no estoque. Por fim, imprimimos o relatório completo do estoque.