Certamente! Aqui está um exemplo de código complexo em Swift, que utiliza várias funcionalidades da linguagem:

```swift
import Foundation

// Definindo uma estrutura de dados para representar um Produto
struct Produto {
    var nome: String
    var preco: Double
    var quantidade: Int
}

// Criando uma função que recebe um array de produtos e retorna o valor total do estoque
func calcularValorTotalEstoque(produtos: [Produto]) -> Double {
    var valorTotal: Double = 0
    
    for produto in produtos {
        valorTotal += produto.preco * Double(produto.quantidade)
    }
    
    return valorTotal
}

// Criando uma função que recebe um array de produtos e retorna um dicionário com a quantidade de cada produto
func contarProdutos(produtos: [Produto]) -> [String: Int] {
    var quantidadeProdutos: [String: Int] = [:]
    
    for produto in produtos {
        if let quantidadeExistente = quantidadeProdutos[produto.nome] {
            quantidadeProdutos[produto.nome] = quantidadeExistente + produto.quantidade
        } else {
            quantidadeProdutos[produto.nome] = produto.quantidade
        }
    }
    
    return quantidadeProdutos
}

// Criando um array de produtos
let produtos: [Produto] = [
    Produto(nome: "Camiseta", preco: 39.99, quantidade: 10),
    Produto(nome: "Calça", preco: 79.99, quantidade: 5),
    Produto(nome: "Tênis", preco: 129.99, quantidade: 3),
    Produto(nome: "Meia", preco: 9.99, quantidade: 20)
]

// Calculando o valor total do estoque
let valorTotalEstoque = calcularValorTotalEstoque(produtos: produtos)
print("O valor total do estoque é R$\(valorTotalEstoque)")

// Contando a quantidade de cada produto
let quantidadeProdutos = contarProdutos(produtos: produtos)
print("Quantidade de produtos:")
for (nome, quantidade) in quantidadeProdutos {
    print("\(nome): \(quantidade)")
}
```

Neste exemplo, temos a definição de uma estrutura de dados `Produto`, que possui nome, preço e quantidade. Em seguida, temos duas funções: `calcularValorTotalEstoque`, que recebe um array de produtos e retorna o valor total do estoque, e `contarProdutos`, que recebe um array de produtos e retorna um dicionário com a quantidade de cada produto.

No final, criamos um array de produtos, chamamos as duas funções para calcular o valor total do estoque e contar a quantidade de cada produto, e imprimimos os resultados.

Espero que esse código complexo em Swift atenda às suas expectativas!