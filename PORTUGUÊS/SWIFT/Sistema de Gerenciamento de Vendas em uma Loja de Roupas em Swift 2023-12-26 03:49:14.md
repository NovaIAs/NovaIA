Certamente, vou gerar um código complexo em Swift para você. Este código é um exemplo de um sistema de gerenciamento de vendas em uma loja de roupas. Ele permite adicionar produtos, calcular o total das vendas e exibir um relatório completo das vendas realizadas. Vamos lá:

```swift
// Definição da estrutura do Produto
struct Produto {
    var nome: String
    var preco: Double
}

// Definição da classe Loja
class Loja {
    var produtos: [Produto] = []
    var vendas: [Produto] = []

    // Método para adicionar produtos à lista de produtos disponíveis na loja
    func adicionarProduto(nome: String, preco: Double) {
        let produto = Produto(nome: nome, preco: preco)
        produtos.append(produto)
        print("Produto \(nome) adicionado com sucesso!")
    }

    // Método para realizar uma venda
    func venderProduto(nome: String) {
        if let produto = produtos.first(where: { $0.nome == nome }) {
            vendas.append(produto)
            print("Produto \(nome) vendido com sucesso!")
        } else {
            print("Produto \(nome) não encontrado na loja.")
        }
    }

    // Método para calcular o total das vendas realizadas
    func calcularTotalVendas() -> Double {
        var total: Double = 0
        for produto in vendas {
            total += produto.preco
        }
        return total
    }

    // Método para exibir um relatório completo das vendas realizadas
    func exibirRelatorioVendas() {
        print("Relatório de Vendas:")
        print("--------------------")
        for produto in vendas {
            print("\(produto.nome) - R$ \(produto.preco)")
        }
        print("--------------------")
        let totalVendas = calcularTotalVendas()
        print("Total de Vendas: R$ \(totalVendas)")
    }
}

// Exemplo de utilização do código
let minhaLoja = Loja()

minhaLoja.adicionarProduto(nome: "Camiseta", preco: 50.0)
minhaLoja.adicionarProduto(nome: "Calça", preco: 100.0)
minhaLoja.adicionarProduto(nome: "Jaqueta", preco: 150.0)

minhaLoja.venderProduto(nome: "Camiseta")
minhaLoja.venderProduto(nome: "Calça")
minhaLoja.venderProduto(nome: "Jaqueta")
minhaLoja.venderProduto(nome: "Sapato") // Produto não encontrado

minhaLoja.exibirRelatorioVendas()
```

Explicação do código:
- Primeiro, definimos a estrutura `Produto` que tem dois atributos: `nome` e `preco`.
- Em seguida, definimos a classe `Loja` que possui duas listas: `produtos` (para armazenar os produtos disponíveis na loja) e `vendas` (para armazenar os produtos vendidos).
- A classe `Loja` possui os seguintes métodos:
  - `adicionarProduto`: recebe o nome e o preço de um produto e adiciona à lista de produtos disponíveis na loja.
  - `venderProduto`: recebe o nome de um produto e o adiciona à lista de vendas, se encontrado na lista de produtos disponíveis. Caso contrário, exibe uma mensagem informando que o produto não foi encontrado.
  - `calcularTotalVendas`: percorre a lista de vendas e retorna o total das vendas realizadas.
  - `exibirRelatorioVendas`: exibe um relatório completo das vendas realizadas, listando os produtos vendidos e o total de vendas.
- Por fim, é feito um exemplo de utilização do código, onde uma loja é criada, alguns produtos são adicionados e vendidos, e um relatório de vendas é exibido.

Espero que este código atenda às suas expectativas!