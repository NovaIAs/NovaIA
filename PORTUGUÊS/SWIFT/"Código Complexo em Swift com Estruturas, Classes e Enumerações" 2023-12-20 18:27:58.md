Claro! Aqui está um código complexo em Swift que utiliza vários recursos e é explicado em detalhes:

```swift
import Foundation

// Definindo uma estrutura chamada Pessoa
struct Pessoa {
    var nome: String
    var idade: Int
    var endereco: String
    
    // Definindo um método da estrutura Pessoa para imprimir os detalhes da pessoa
    func imprimirDetalhes() {
        print("Nome: \(nome)")
        print("Idade: \(idade)")
        print("Endereço: \(endereco)")
    }
}

// Criando uma instância da estrutura Pessoa
var pessoa1 = Pessoa(nome: "João", idade: 25, endereco: "Rua A, 123")
pessoa1.imprimirDetalhes()

// Definindo uma classe chamada Animal
class Animal {
    var nome: String
    var idade: Int
    var especie: String
    
    // Definindo um inicializador da classe Animal
    init(nome: String, idade: Int, especie: String) {
        self.nome = nome
        self.idade = idade
        self.especie = especie
    }
    
    // Definindo um método da classe Animal para imprimir os detalhes do animal
    func imprimirDetalhes() {
        print("Nome: \(nome)")
        print("Idade: \(idade)")
        print("Espécie: \(especie)")
    }
}

// Criando uma instância da classe Animal
var animal1 = Animal(nome: "Rex", idade: 5, especie: "Cachorro")
animal1.imprimirDetalhes()

// Definindo uma enumeração chamada EstadoCivil
enum EstadoCivil: String {
    case solteiro = "Solteiro(a)"
    case casado = "Casado(a)"
    case divorciado = "Divorciado(a)"
    case viuvo = "Viúvo(a)"
}

// Criando uma função que recebe um estado civil como parâmetro e retorna uma mensagem personalizada
func mensagemEstadoCivil(estadoCivil: EstadoCivil) -> String {
    switch estadoCivil {
    case .solteiro:
        return "Você é solteiro(a)."
    case .casado:
        return "Você é casado(a)."
    case .divorciado:
        return "Você é divorciado(a)."
    case .viuvo:
        return "Você é viúvo(a)."
    }
}

// Chamando a função mensagemEstadoCivil com diferentes valores da enumeração EstadoCivil
print(mensagemEstadoCivil(estadoCivil: .solteiro))
print(mensagemEstadoCivil(estadoCivil: .casado))
print(mensagemEstadoCivil(estadoCivil: .divorciado))
print(mensagemEstadoCivil(estadoCivil: .viuvo))
```

Explicação do código:

- O código começa importando a biblioteca Foundation, que fornece funcionalidades adicionais para o aplicativo Swift.

- Em seguida, é definida uma estrutura chamada Pessoa com três propriedades: nome, idade e endereco. A estrutura também possui um método chamado imprimirDetalhes, que imprime as informações da pessoa.

- É criada uma instância da estrutura Pessoa chamada pessoa1 e, em seguida, é chamado o método imprimirDetalhes para exibir os detalhes da pessoa.

- Em seguida, é definida uma classe chamada Animal com três propriedades: nome, idade e especie. A classe também possui um inicializador personalizado e um método chamado imprimirDetalhes, que imprime as informações do animal.

- É criada uma instância da classe Animal chamada animal1 e, em seguida, é chamado o método imprimirDetalhes para exibir os detalhes do animal.

- Depois, é definida uma enumeração chamada EstadoCivil com quatro casos: solteiro, casado, divorciado e viuvo. Cada caso possui um valor associado do tipo String.

- É criada uma função chamada mensagemEstadoCivil que recebe um valor da enumeração EstadoCivil como parâmetro e retorna uma mensagem personalizada com base no estado civil fornecido.

- A função mensagemEstadoCivil é chamada quatro vezes, passando cada um dos casos da enumeração EstadoCivil como argumento, e as mensagens são impressas no console.

Espero que esse código complexo em Swift atenda às suas expectativas! Se tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.