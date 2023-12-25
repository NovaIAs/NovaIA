Claro! Aqui está um exemplo de um código complexo em Swift, que utiliza várias funcionalidades da linguagem:

```swift
import Foundation

// Definição da estrutura de dados
struct Pessoa {
    var nome: String
    var idade: Int
}

// Classe que representa uma lista de pessoas
class ListaPessoas {
    private var pessoas: [Pessoa] = []
    
    // Método para adicionar uma pessoa à lista
    func adicionarPessoa(nome: String, idade: Int) {
        let pessoa = Pessoa(nome: nome, idade: idade)
        pessoas.append(pessoa)
    }
    
    // Método para buscar uma pessoa pelo nome
    func buscarPessoa(nome: String) -> Pessoa? {
        for pessoa in pessoas {
            if pessoa.nome == nome {
                return pessoa
            }
        }
        return nil
    }
    
    // Método para calcular a média de idade das pessoas na lista
    func calcularMediaIdade() -> Double {
        var somaIdade = 0
        for pessoa in pessoas {
            somaIdade += pessoa.idade
        }
        return Double(somaIdade) / Double(pessoas.count)
    }
}

// Exemplo de uso da classe ListaPessoas
let lista = ListaPessoas()

lista.adicionarPessoa(nome: "João", idade: 25)
lista.adicionarPessoa(nome: "Maria", idade: 30)
lista.adicionarPessoa(nome: "Carlos", idade: 40)

if let pessoa = lista.buscarPessoa(nome: "João") {
    print("Encontrado: \(pessoa.nome), \(pessoa.idade) anos")
} else {
    print("Pessoa não encontrada")
}

let mediaIdade = lista.calcularMediaIdade()
print("Média de idade: \(mediaIdade) anos")
```

Neste código, é definida uma estrutura de dados chamada Pessoa, que contém os atributos nome e idade. Em seguida, é criada uma classe chamada ListaPessoas, que possui um array de pessoas e métodos para adicionar uma pessoa à lista, buscar uma pessoa pelo nome e calcular a média de idade das pessoas na lista.

O exemplo de uso da classe ListaPessoas cria uma instância da classe, adiciona algumas pessoas à lista, busca uma pessoa pelo nome "João" e calcula a média de idade de todas as pessoas na lista.

Esse código é apenas um exemplo, e é possível adaptá-lo conforme as necessidades específicas do projeto.