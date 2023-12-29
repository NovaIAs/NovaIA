Certamente! Aqui está um exemplo de um código complexo em Swift que realiza a análise e processamento de dados em uma lista de itens de compras:

```swift
import Foundation

// Definindo a estrutura de um item de compra
struct ItemCompra {
    let nome: String
    let quantidade: Int
    let precoUnitario: Double
}

// Definindo a lista de itens de compra
var listaDeCompras: [ItemCompra] = []

// Função para adicionar um item à lista de compras
func adicionarItem(nome: String, quantidade: Int, precoUnitario: Double) {
    let item = ItemCompra(nome: nome, quantidade: quantidade, precoUnitario: precoUnitario)
    listaDeCompras.append(item)
}

// Função para calcular o valor total da lista de compras
func calcularValorTotal() -> Double {
    var valorTotal: Double = 0
    for item in listaDeCompras {
        valorTotal += Double(item.quantidade) * item.precoUnitario
    }
    return valorTotal
}

// Função para imprimir os itens da lista de compras
func imprimirListaDeCompras() {
    print("Lista de Compras:")
    for item in listaDeCompras {
        print("\(item.nome) - Quantidade: \(item.quantidade) - Preço Unitário: R$ \(item.precoUnitario)")
    }
    print("Valor Total: R$ \(calcularValorTotal())")
}

// Adicionando itens à lista de compras
adicionarItem(nome: "Maçã", quantidade: 5, precoUnitario: 2.50)
adicionarItem(nome: "Banana", quantidade: 3, precoUnitario: 1.20)
adicionarItem(nome: "Laranja", quantidade: 4, precoUnitario: 2.0)

// Imprimindo a lista de compras
imprimirListaDeCompras()
```

Neste código, começamos importando o framework `Foundation` para utilizar recursos adicionais. Em seguida, definimos uma estrutura `ItemCompra` para representar um item da lista de compras, com propriedades como nome, quantidade e preço unitário.

Em seguida, criamos uma lista vazia `listaDeCompras` do tipo `ItemCompra`. A função `adicionarItem` permite adicionar um item à lista de compras, criando uma instância de `ItemCompra` com os parâmetros fornecidos e adicionando-a à lista.

A função `calcularValorTotal` percorre a lista de compras e calcula o valor total multiplicando a quantidade pelo preço unitário de cada item.

Por fim, a função `imprimirListaDeCompras` exibe na tela os itens da lista de compras, juntamente com suas quantidades e preços unitários, além do valor total calculado.

No exemplo, adicionamos três itens à lista de compras usando a função `adicionarItem`, e em seguida, chamamos a função `imprimirListaDeCompras` para exibir os itens e o valor total calculado.

Espero que este código complexo em Swift atenda às suas expectativas!