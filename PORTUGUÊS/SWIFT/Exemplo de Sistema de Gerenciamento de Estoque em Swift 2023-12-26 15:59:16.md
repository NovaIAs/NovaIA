Claro! Aqui está um exemplo de um código complexo em Swift, que implementa um sistema de gerenciamento de estoque para uma loja fictícia:

```swift
import Foundation

// Definindo a estrutura de um produto
struct Produto {
    var nome: String
    var preco: Double
    var quantidade: Int
}

// Definindo a classe de Gerenciamento de Estoque
class Estoque {
    var produtos: [Produto]
    
    init() {
        self.produtos = []
    }
    
    // Adicionar um produto ao estoque
    func adicionarProduto(nome: String, preco: Double, quantidade: Int) {
        let novoProduto = Produto(nome: nome, preco: preco, quantidade: quantidade)
        produtos.append(novoProduto)
        print("Produto \(novoProduto.nome) adicionado ao estoque.")
    }
    
    // Remover um produto do estoque
    func removerProduto(nome: String) {
        if let index = produtos.firstIndex(where: { $0.nome == nome }) {
            produtos.remove(at: index)
            print("Produto \(nome) removido do estoque.")
        } else {
            print("Produto \(nome) não encontrado no estoque.")
        }
    }
    
    // Atualizar a quantidade de um produto no estoque
    func atualizarQuantidade(nome: String, quantidade: Int) {
        if let produto = produtos.first(where: { $0.nome == nome }) {
            produto.quantidade += quantidade
            print("Quantidade do produto \(produto.nome) atualizada: \(produto.quantidade).")
        } else {
            print("Produto \(nome) não encontrado no estoque.")
        }
    }
    
    // Listar todos os produtos no estoque
    func listarProdutos() {
        if produtos.isEmpty {
            print("O estoque está vazio.")
        } else {
            print("Produtos no estoque:")
            for produto in produtos {
                print("- \(produto.nome): R$ \(produto.preco) (Quantidade: \(produto.quantidade))")
            }
        }
    }
}

// Exemplo de uso do gerenciamento de estoque
let estoque = Estoque()

estoque.listarProdutos()

estoque.adicionarProduto(nome: "Camiseta", preco: 29.99, quantidade: 10)
estoque.adicionarProduto(nome: "Calça Jeans", preco: 79.99, quantidade: 5)

estoque.listarProdutos()

estoque.atualizarQuantidade(nome: "Camiseta", quantidade: -3)
estoque.atualizarQuantidade(nome: "Calça Jeans", quantidade: 2)

estoque.listarProdutos()

estoque.removerProduto(nome: "Camiseta")
estoque.removerProduto(nome: "Chinelo")

estoque.listarProdutos()
```

Neste código, começamos definindo a estrutura `Produto`, que possui propriedades como `nome`, `preco` e `quantidade`. Em seguida, criamos a classe `Estoque`, que possui um array de produtos como propriedade.

Dentro da classe `Estoque`, temos métodos para adicionar um produto ao estoque, remover um produto do estoque, atualizar a quantidade de um produto e listar todos os produtos no estoque. Cada método realiza as operações necessárias e imprime mensagens informativas para o usuário.

No exemplo de uso, criamos uma instância do `Estoque` e realizamos algumas operações, como adicionar produtos, atualizar quantidades e remover produtos. Em cada etapa, chamamos o método apropriado e exibimos os resultados na saída do console.

Espero que este exemplo tenha sido útil para você!