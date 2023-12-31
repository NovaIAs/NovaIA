Claro! Aqui está um exemplo de código em Swift que lida com uma lista de contatos:

```swift
import Foundation

struct Contato {
    var nome: String
    var telefone: String
}

class Agenda {
    var contatos: [Contato] = []
    
    func adicionarContato(nome: String, telefone: String) {
        let contato = Contato(nome: nome, telefone: telefone)
        contatos.append(contato)
    }
    
    func removerContato(nome: String) {
        contatos = contatos.filter { $0.nome != nome }
    }
    
    func buscarContato(nome: String) -> Contato? {
        return contatos.first { $0.nome == nome }
    }
    
    func listarContatos() {
        if contatos.isEmpty {
            print("Agenda vazia.")
        } else {
            for contato in contatos {
                print("Nome: \(contato.nome) - Telefone: \(contato.telefone)")
            }
        }
    }
}

// Exemplo de uso da agenda
let agenda = Agenda()

agenda.adicionarContato(nome: "João", telefone: "(11) 1234-5678")
agenda.adicionarContato(nome: "Maria", telefone: "(11) 8765-4321")
agenda.adicionarContato(nome: "Pedro", telefone: "(11) 5555-5555")

agenda.listarContatos()

agenda.removerContato(nome: "Maria")

let contatoBuscado = agenda.buscarContato(nome: "João")
if let contato = contatoBuscado {
    print("Contato encontrado: \(contato.nome) - \(contato.telefone)")
} else {
    print("Contato não encontrado.")
}
```

Neste código, temos a definição de uma estrutura `Contato` que possui dois atributos: `nome` e `telefone`. Em seguida, temos a classe `Agenda`, que possui um array de `Contato` chamado `contatos`. 

Na classe `Agenda`, temos os métodos `adicionarContato`, `removerContato`, `buscarContato` e `listarContatos`. O método `adicionarContato` cria uma nova instância de `Contato` com os parâmetros informados e a adiciona ao array `contatos`. O método `removerContato` remove um contato do array `contatos` caso o nome informado seja igual ao nome de algum contato já existente. O método `buscarContato` retorna o primeiro contato encontrado com o nome informado ou `nil` caso nenhum contato seja encontrado. O método `listarContatos` imprime na tela todos os contatos presentes no array `contatos`.

No exemplo de uso da agenda, criamos uma instância de `Agenda` e adicionamos alguns contatos. Em seguida, listamos todos os contatos presentes na agenda, removemos um contato pelo nome e buscamos um contato pelo nome. Por fim, imprimimos na tela o resultado da busca.