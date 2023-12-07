Certamente! Aqui está um código em Swift, bastante complexo e diferenciado, escrito em português:

```swift
import Foundation

// Definindo a classe "Pessoa"
class Pessoa {
    let nome: String
    var idade: Int
    
    init(nome: String, idade: Int) {
        self.nome = nome
        self.idade = idade
    }
    
    func cumprimentar() {
        print("Olá, meu nome é \(nome) e tenho \(idade) anos.")
    }
}

// Definindo a classe "Aluno", que herda da classe "Pessoa"
class Aluno: Pessoa {
    let numeroMatricula: Int
    
    init(nome: String, idade: Int, numeroMatricula: Int) {
        self.numeroMatricula = numeroMatricula
        super.init(nome: nome, idade: idade)
    }
    
    override func cumprimentar() {
        super.cumprimentar()
        print("Sou aluno e meu número de matrícula é \(numeroMatricula).")
    }
    
    func estudar() {
        print("Estou estudando.")
    }
}

// Definindo a classe "Professor", que herda da classe "Pessoa"
class Professor: Pessoa {
    let especialidade: String
    
    init(nome: String, idade: Int, especialidade: String) {
        self.especialidade = especialidade
        super.init(nome: nome, idade: idade)
    }
    
    override func cumprimentar() {
        super.cumprimentar()
        print("Sou professor e minha especialidade é \(especialidade).")
    }
    
    func ensinar() {
        print("Estou ensinando.")
    }
}

// Criando uma instância da classe "Aluno"
let joao = Aluno(nome: "João", idade: 20, numeroMatricula: 12345)
joao.cumprimentar()
joao.estudar()

// Criando uma instância da classe "Professor"
let maria = Professor(nome: "Maria", idade: 35, especialidade: "Matemática")
maria.cumprimentar()
maria.ensinar()
```

Explicação do código:
- Começamos definindo a classe "Pessoa", que possui duas propriedades: "nome" e "idade". Também há um inicializador para definir essas propriedades.
- Em seguida, temos o método "cumprimentar" da classe "Pessoa", que imprime uma mensagem com o nome e idade da pessoa.
- Depois, definimos a classe "Aluno", que herda da classe "Pessoa". Ela adiciona a propriedade "numeroMatricula" e tem um inicializador próprio que chama o inicializador da superclasse.
- A classe "Aluno" também possui o método "cumprimentar", que chama o método da superclasse e adiciona uma mensagem com o número de matrícula.
- Além disso, há o método "estudar" que imprime uma mensagem informando que o aluno está estudando.
- Em seguida, definimos a classe "Professor", que também herda da classe "Pessoa". Aqui adicionamos a propriedade "especialidade" e um inicializador próprio.
- A classe "Professor" sobrescreve o método "cumprimentar" da superclasse e adiciona uma mensagem com a especialidade do professor.
- Além disso, há o método "ensinar" que imprime uma mensagem informando que o professor está ensinando.
- Por fim, criamos uma instância da classe "Aluno" e da classe "Professor", e chamamos os métodos correspondentes.

Espero que este código atenda às suas necessidades! Caso precise de mais alguma coisa, estou à disposição.