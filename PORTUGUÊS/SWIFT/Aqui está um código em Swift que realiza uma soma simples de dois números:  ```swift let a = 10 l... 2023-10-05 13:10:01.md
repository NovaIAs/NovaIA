Claro! Aqui está um exemplo de um código complexo em Swift que utiliza recursos avançados da linguagem:

```swift
import Foundation

// Definindo uma classe chamada Pessoa
class Pessoa {
    var nome: String
    var idade: Int

    init(nome: String, idade: Int) {
        self.nome = nome
        self.idade = idade
    }

    func saudacao() {
        print("Olá, meu nome é \(nome) e eu tenho \(idade) anos.")
    }
}

// Criando uma instância da classe Pessoa
let pessoa = Pessoa(nome: "João", idade: 25)

// Chamando o método saudacao da instância
pessoa.saudacao()

// Definindo uma extensão para a classe Pessoa
extension Pessoa {
    func parabenizar() {
        print("Parabéns, \(nome)! Hoje é o seu aniversário de \(idade) anos.")
    }
}

// Chamando o método parabenizar da instância
pessoa.parabenizar()

// Definindo um protocolo chamado PodeComemorar
protocol PodeComemorar {
    func comemorar()
}

// Estendendo a classe Pessoa para conformar ao protocolo PodeComemorar
extension Pessoa: PodeComemorar {
    func comemorar() {
        print("\(nome) está comemorando seu aniversário de \(idade) anos!")
    }
}

// Criando uma nova instância da classe Pessoa
let outraPessoa = Pessoa(nome: "Maria", idade: 30)

// Chamando o método comemorar da nova instância
outraPessoa.comemorar()
```

Neste exemplo, começamos definindo uma classe chamada "Pessoa" que possui duas propriedades: "nome" (do tipo String) e "idade" (do tipo Int). Também definimos um inicializador para a classe.

Em seguida, criamos uma instância da classe Pessoa chamada "pessoa" e chamamos o método "saudacao", que imprime uma saudação personalizada na tela.

Depois, estendemos a classe Pessoa com um novo método chamado "parabenizar", que imprime uma mensagem de parabéns no aniversário da pessoa.

Em seguida, definimos um protocolo chamado "PodeComemorar" que possui um método "comemorar". Estendemos a classe Pessoa para conformar a esse protocolo, implementando o método "comemorar".

Por fim, criamos uma nova instância da classe Pessoa chamada "outraPessoa" e chamamos o método "comemorar" dessa nova instância.

Esse código demonstra alguns recursos avançados da linguagem Swift, como classes, propriedades, métodos, inicializadores, extensões e protocolos.