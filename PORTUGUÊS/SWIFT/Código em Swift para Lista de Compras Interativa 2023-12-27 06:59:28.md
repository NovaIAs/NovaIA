Claro! Aqui está um código complexo em Swift que envolve a criação de uma lista de compras interativa. O código está dividido em seções com comentários explicativos para facilitar o entendimento:

```swift
import Foundation

// Criar uma classe para representar um item de compra
class ItemDeCompra {
    var nome: String
    var quantidade: Int
    var preco: Double
    
    init(nome: String, quantidade: Int, preco: Double) {
        self.nome = nome
        self.quantidade = quantidade
        self.preco = preco
    }
}

// Criar uma função para adicionar itens à lista de compras
func adicionarItem(lista: inout [ItemDeCompra]) {
    print("Digite o nome do item:")
    let nome = readLine() ?? ""
    
    print("Digite a quantidade:")
    let quantidadeString = readLine() ?? ""
    let quantidade = Int(quantidadeString) ?? 0
    
    print("Digite o preço:")
    let precoString = readLine() ?? ""
    let preco = Double(precoString) ?? 0.0
    
    let item = ItemDeCompra(nome: nome, quantidade: quantidade, preco: preco)
    lista.append(item)
    
    print("Item adicionado à lista de compras!")
}

// Criar uma função para exibir a lista de compras
func exibirLista(lista: [ItemDeCompra]) {
    if lista.isEmpty {
        print("A lista de compras está vazia.")
    } else {
        print("Lista de compras:")
        for item in lista {
            print("\(item.quantidade) x \(item.nome) - R$ \(item.preco)")
        }
    }
}

// Criar uma função para calcular o total da lista de compras
func calcularTotal(lista: [ItemDeCompra]) -> Double {
    var total: Double = 0.0
    
    for item in lista {
        total += item.preco * Double(item.quantidade)
    }
    
    return total
}

// Criar uma função para remover um item da lista de compras
func removerItem(lista: inout [ItemDeCompra]) {
    exibirLista(lista: lista)
    
    print("Digite o número do item que deseja remover:")
    let indexString = readLine() ?? ""
    let index = Int(indexString) ?? 0
    
    if index >= 0 && index < lista.count {
        lista.remove(at: index)
        print("Item removido da lista de compras!")
    } else {
        print("Número de item inválido.")
    }
}

// Função principal
func main() {
    var listaDeCompras: [ItemDeCompra] = []

    var continuar = true
    while continuar {
        // Exibir opções para o usuário
        print("\nO que você deseja fazer?")
        print("1. Adicionar item à lista de compras")
        print("2. Exibir lista de compras")
        print("3. Calcular total da lista de compras")
        print("4. Remover item da lista de compras")
        print("5. Sair")
        
        // Ler a opção do usuário
        let opcaoString = readLine() ?? ""
        let opcao = Int(opcaoString) ?? 0
        
        switch opcao {
        case 1:
            adicionarItem(lista: &listaDeCompras)
        case 2:
            exibirLista(lista: listaDeCompras)
        case 3:
            let total = calcularTotal(lista: listaDeCompras)
            print("Total da lista de compras: R$ \(total)")
        case 4:
            removerItem(lista: &listaDeCompras)
        case 5:
            continuar = false
        default:
            print("Opção inválida.")
        }
    }
}

// Chamar a função principal para iniciar o programa
main()
```

Neste código, você pode adicionar itens à lista de compras, exibir a lista, calcular o total dos itens e remover itens da lista. O programa continua em execução até que você escolha a opção de sair.