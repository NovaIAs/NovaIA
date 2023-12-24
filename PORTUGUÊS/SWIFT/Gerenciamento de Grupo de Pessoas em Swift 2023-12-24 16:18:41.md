import Foundation

// Definindo uma estrutura de dados para representar uma pessoa
struct Pessoa {
    var nome: String
    var idade: Int
}

// Definindo uma classe para gerenciar um grupo de pessoas
class GerenciadorPessoas {
    var pessoas: [Pessoa]
    
    init(pessoas: [Pessoa]) {
        self.pessoas = pessoas
    }
    
    // Função para adicionar uma pessoa ao grupo
    func adicionarPessoa(pessoa: Pessoa) {
        pessoas.append(pessoa)
    }
    
    // Função para remover uma pessoa do grupo
    func removerPessoa(nome: String) {
        pessoas = pessoas.filter { $0.nome != nome }
    }
    
    // Função para obter a média de idade do grupo
    func obterMediaIdade() -> Double {
        let somaIdades = pessoas.reduce(0) { $0 + $1.idade }
        return Double(somaIdades) / Double(pessoas.count)
    }
    
    // Função para obter a pessoa mais velha do grupo
    func obterPessoaMaisVelha() -> Pessoa? {
        return pessoas.max(by: { $0.idade < $1.idade })
    }
}

// Criando algumas pessoas
let pessoa1 = Pessoa(nome: "João", idade: 25)
let pessoa2 = Pessoa(nome: "Maria", idade: 30)
let pessoa3 = Pessoa(nome: "Pedro", idade: 20)

// Criando um grupo de pessoas
let grupoPessoas = GerenciadorPessoas(pessoas: [pessoa1, pessoa2, pessoa3])

// Adicionando uma pessoa ao grupo
grupoPessoas.adicionarPessoa(pessoa: Pessoa(nome: "José", idade: 28))

// Removendo uma pessoa do grupo
grupoPessoas.removerPessoa(nome: "Maria")

// Obtendo a média de idade do grupo
let mediaIdade = grupoPessoas.obterMediaIdade()
print("A média de idade do grupo é: \(mediaIdade)")

// Obtendo a pessoa mais velha do grupo
if let pessoaMaisVelha = grupoPessoas.obterPessoaMaisVelha() {
    print("A pessoa mais velha do grupo é: \(pessoaMaisVelha.nome)")
} else {
    print("Não há pessoas no grupo.")
}
```

Neste código em Swift, criamos uma estrutura de dados chamada `Pessoa` para representar uma pessoa com um nome e uma idade. Em seguida, definimos uma classe chamada `GerenciadorPessoas` que possui um array de pessoas e algumas funções para adicionar e remover pessoas do grupo, obter a média de idade e encontrar a pessoa mais velha.

No exemplo, criamos algumas pessoas e um grupo de pessoas usando o `GerenciadorPessoas`. Em seguida, adicionamos uma pessoa ao grupo, removemos outra pessoa do grupo e calculamos a média de idade do grupo. Por fim, encontramos a pessoa mais velha do grupo e exibimos o resultado na tela.

Este código em Swift é um exemplo simples de como gerenciar um grupo de pessoas e realizar algumas operações básicas com os dados. Você pode adaptá-lo e adicionar mais funcionalidades de acordo com suas necessidades.