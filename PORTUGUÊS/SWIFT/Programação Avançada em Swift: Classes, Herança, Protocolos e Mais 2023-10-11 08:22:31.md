Certamente! Aqui está um código complexo em Swift, que utiliza várias funcionalidades avançadas e é explicado em detalhes:

```swift
import UIKit

// Definindo uma classe chamada Pessoa
class Pessoa {
    var nome: String
    var idade: Int
    
    init(nome: String, idade: Int) {
        self.nome = nome
        self.idade = idade
    }
    
    func cumprimentar() {
        print("Olá, meu nome é \(nome) e tenho \(idade) anos.")
    }
}

// Definindo uma extensão para a classe Pessoa
extension Pessoa {
    func fazerAniversario() {
        idade += 1
        print("Feliz aniversário! Agora tenho \(idade) anos.")
    }
}

// Criando uma instância da classe Pessoa
let pessoa = Pessoa(nome: "João", idade: 25)

// Chamando o método cumprimentar da instância
pessoa.cumprimentar()

// Chamando o método fazerAniversario da instância
pessoa.fazerAniversario()

// Definindo uma classe chamada Aluno que herda da classe Pessoa
class Aluno: Pessoa {
    var numeroMatricula: Int
    
    init(nome: String, idade: Int, numeroMatricula: Int) {
        self.numeroMatricula = numeroMatricula
        super.init(nome: nome, idade: idade)
    }
    
    override func cumprimentar() {
        print("Olá, meu nome é \(nome), tenho \(idade) anos e sou o aluno número \(numeroMatricula).")
    }
}

// Criando uma instância da classe Aluno
let aluno = Aluno(nome: "Maria", idade: 20, numeroMatricula: 123456)

// Chamando o método cumprimentar da instância de Aluno
aluno.cumprimentar()

// Implementando uma closure que retorna a soma de dois números
let soma: (Int, Int) -> Int = { num1, num2 in
    return num1 + num2
}

// Chamando a closure para obter o resultado da soma
let resultado = soma(5, 3)
print("O resultado da soma é \(resultado).")

// Definindo uma função genérica que retorna o maior elemento de um array
func encontrarMaiorElemento<T: Comparable>(em array: [T]) -> T? {
    guard let primeiroElemento = array.first else {
        return nil
    }
    
    var maiorElemento = primeiroElemento
    
    for elemento in array {
        if elemento > maiorElemento {
            maiorElemento = elemento
        }
    }
    
    return maiorElemento
}

// Criando um array de números inteiros
let numerosInteiros = [1, 5, 3, 8, 2]

// Chamando a função encontrarMaiorElemento para obter o maior número do array
if let maiorNumero = encontrarMaiorElemento(em: numerosInteiros) {
    print("O maior número do array é \(maiorNumero).")
} else {
    print("O array está vazio.")
}

// Definindo um protocolo chamado Imprimivel
protocol Imprimivel {
    func imprimir()
}

// Implementando o protocolo Imprimivel na classe Pessoa
extension Pessoa: Imprimivel {
    func imprimir() {
        print("Imprimindo pessoa: \(nome), \(idade) anos.")
    }
}

// Implementando o protocolo Imprimivel na classe Aluno
extension Aluno: Imprimivel {
    func imprimir() {
        print("Imprimindo aluno: \(nome), \(idade) anos, matrícula \(numeroMatricula).")
    }
}

// Criando um array de objetos que implementam o protocolo Imprimivel
let objetosImprimiveis: [Imprimivel] = [pessoa, aluno]

// Iterando sobre o array e chamando o método imprimir de cada objeto
for objeto in objetosImprimiveis {
    objeto.imprimir()
}
```

Este código em Swift é bastante complexo e possui várias funcionalidades avançadas:

1. Criação de classes e utilização de propriedades
2. Uso de inicializadores e métodos de instância
3. Definição de uma extensão para adicionar métodos a uma classe existente
4. Herança de classes
5. Sobrescrita de métodos
6. Uso de closures para realizar operações matemáticas
7. Implementação de uma função genérica para encontrar o maior elemento de um array
8. Definição de um protocolo e implementação do protocolo em classes
9. Utilização de arrays para armazenar objetos de diferentes tipos
10. Iteração sobre um array e chamada de métodos específicos de cada objeto

Espero que este código complexo possa te ajudar a aprender e explorar as funcionalidades avançadas da linguagem Swift!