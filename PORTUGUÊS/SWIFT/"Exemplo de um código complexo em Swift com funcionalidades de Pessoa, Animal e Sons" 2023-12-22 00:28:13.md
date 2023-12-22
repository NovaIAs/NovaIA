Certamente! Aqui está um exemplo de um código complexo em Swift que inclui várias funcionalidades e explicação em português:

```swift
import UIKit

// Definindo uma classe para representar uma pessoa
class Pessoa {
    var nome: String
    var idade: Int
    
    init(nome: String, idade: Int) {
        self.nome = nome
        self.idade = idade
    }
    
    func cumprimentar() {
        print("Olá, meu nome é \(nome) e eu tenho \(idade) anos.")
    }
}

// Criando uma instância da classe Pessoa
let pessoa = Pessoa(nome: "João", idade: 25)

// Chamando o método cumprimentar da instância
pessoa.cumprimentar()

// Definindo uma extensão para a classe Pessoa
extension Pessoa {
    func fazerAniversario() {
        idade += 1
        print("Parabéns \(nome)! Agora você tem \(idade) anos.")
    }
}

// Chamando o método fazerAniversario da instância
pessoa.fazerAniversario()

// Definindo um protocolo para representar um animal
protocol Animal {
    var nome: String { get }
    
    func emitirSom()
}

// Definindo uma enumeração para representar diferentes tipos de animais
enum TipoAnimal {
    case cachorro, gato, passaro
}

// Criando uma classe que implementa o protocolo Animal
class Cachorro: Animal {
    var nome: String
    
    init(nome: String) {
        self.nome = nome
    }
    
    func emitirSom() {
        print("\(nome) está latindo: Au au!")
    }
}

// Criando uma classe que implementa o protocolo Animal
class Gato: Animal {
    var nome: String
    
    init(nome: String) {
        self.nome = nome
    }
    
    func emitirSom() {
        print("\(nome) está miando: Miau!")
    }
}

// Criando uma classe que implementa o protocolo Animal
class Passaro: Animal {
    var nome: String
    
    init(nome: String) {
        self.nome = nome
    }
    
    func emitirSom() {
        print("\(nome) está cantando: Piu piu!")
    }
}

// Função genérica que recebe um tipo de animal e chama o método emitirSom
func fazerBarulho<T: Animal>(para animal: T) {
    animal.emitirSom()
}

// Criando instâncias das classes de animais
let cachorro = Cachorro(nome: "Rex")
let gato = Gato(nome: "Miau")
let passaro = Passaro(nome: "Piu")

// Chamando a função fazerBarulho com diferentes instâncias de animais
fazerBarulho(para: cachorro)
fazerBarulho(para: gato)
fazerBarulho(para: passaro)
```

Neste código, temos a definição de uma classe `Pessoa`, que possui propriedades `nome` e `idade`, além de um método `cumprimentar` que imprime uma mensagem de cumprimento com o nome e idade da pessoa.

Em seguida, é definida uma extensão para a classe `Pessoa`, adicionando um método `fazerAniversario` que incrementa a idade da pessoa em 1 e imprime uma mensagem de parabéns.

Após isso, é definido um protocolo `Animal`, que possui uma propriedade `nome` e um método `emitirSom`. Em seguida, são definidas três classes que implementam esse protocolo: `Cachorro`, `Gato` e `Passaro`. Cada uma dessas classes possui uma implementação diferente para o método `emitirSom`.

Por fim, é definida uma função genérica `fazerBarulho` que recebe uma instância de qualquer classe que implemente o protocolo `Animal` e chama o método `emitirSom` dessa instância.

O código então cria instâncias das classes `Cachorro`, `Gato` e `Passaro` e chama a função `fazerBarulho` passando cada uma dessas instâncias como argumento, resultando na impressão dos sons característicos de cada animal.