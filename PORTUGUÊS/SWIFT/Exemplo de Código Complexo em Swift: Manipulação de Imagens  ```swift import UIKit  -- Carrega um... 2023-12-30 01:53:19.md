Claro! Aqui está um exemplo de um código complexo em Swift, escrito em português, que utiliza várias funcionalidades avançadas da linguagem:

```swift
import Foundation

// Definindo uma estrutura para representar uma pessoa
struct Pessoa {
    var nome: String
    var idade: Int
    var altura: Double
}

// Definindo uma classe para representar uma lista de pessoas
class ListaDePessoas {
    private var pessoas: [Pessoa] = []
    
    // Método para adicionar uma pessoa à lista
    func adicionarPessoa(_ pessoa: Pessoa) {
        pessoas.append(pessoa)
    }
    
    // Método para remover uma pessoa da lista
    func removerPessoa(comNome nome: String) {
        pessoas.removeAll { $0.nome == nome }
    }
    
    // Método para verificar se uma pessoa existe na lista
    func pessoaExiste(comNome nome: String) -> Bool {
        return pessoas.contains { $0.nome == nome }
    }
    
    // Método para obter a média de idade das pessoas na lista
    func mediaIdade() -> Double {
        let totalIdade = pessoas.reduce(0) { $0 + $1.idade }
        return Double(totalIdade) / Double(pessoas.count)
    }
    
    // Método para obter a pessoa mais alta da lista
    func pessoaMaisAlta() -> Pessoa? {
        return pessoas.max(by: { $0.altura < $1.altura })
    }
}

// Exemplo de utilização da classe ListaDePessoas
let lista = ListaDePessoas()

let pessoa1 = Pessoa(nome: "João", idade: 25, altura: 1.75)
let pessoa2 = Pessoa(nome: "Maria", idade: 30, altura: 1.60)
let pessoa3 = Pessoa(nome: "Pedro", idade: 40, altura: 1.80)

lista.adicionarPessoa(pessoa1)
lista.adicionarPessoa(pessoa2)
lista.adicionarPessoa(pessoa3)

print("Média de idade: \(lista.mediaIdade())")

if let pessoaMaisAlta = lista.pessoaMaisAlta() {
    print("Pessoa mais alta: \(pessoaMaisAlta.nome)")
}

lista.removerPessoa(comNome: "Maria")

print("A pessoa 'Maria' existe na lista? \(lista.pessoaExiste(comNome: "Maria"))")
```

Neste exemplo, criamos uma estrutura `Pessoa` com propriedades como nome, idade e altura. Em seguida, criamos uma classe `ListaDePessoas` que possui métodos para adicionar, remover e verificar a existência de pessoas na lista, calcular a média de idade e obter a pessoa mais alta.

No exemplo de utilização da classe `ListaDePessoas`, criamos três pessoas, as adicionamos à lista, calculamos a média de idade e exibimos a pessoa mais alta. Em seguida, removemos uma pessoa da lista e verificamos se ela ainda existe.

Espero que este exemplo atenda às suas expectativas! Caso precise de mais algum código complexo, é só pedir.