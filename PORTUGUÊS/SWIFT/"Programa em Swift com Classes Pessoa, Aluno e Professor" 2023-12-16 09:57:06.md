Certamente! Aqui está um código complexo em Swift, que utiliza conceitos avançados da linguagem:

```
// Classe Pessoa
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

// Classe Aluno, herda de Pessoa
class Aluno: Pessoa {
    var matricula: Int
    var notas: [Double]
    
    init(nome: String, idade: Int, matricula: Int, notas: [Double]) {
        self.matricula = matricula
        self.notas = notas
        
        super.init(nome: nome, idade: idade)
    }
    
    func calcularMedia() -> Double {
        let somaNotas = notas.reduce(0, +)
        let media = somaNotas / Double(notas.count)
        return media
    }
    
    override func cumprimentar() {
        super.cumprimentar()
        print("Eu sou um aluno e minha matrícula é \(matricula).")
    }
}

// Classe Professor, herda de Pessoa
class Professor: Pessoa {
    var disciplina: String
    
    init(nome: String, idade: Int, disciplina: String) {
        self.disciplina = disciplina
        
        super.init(nome: nome, idade: idade)
    }
    
    func ministrarAula() {
        print("Hoje vamos estudar \(disciplina).")
    }
    
    override func cumprimentar() {
        super.cumprimentar()
        print("Eu sou um professor da disciplina de \(disciplina).")
    }
}

// Utilização das classes
let aluno = Aluno(nome: "João", idade: 20, matricula: 123, notas: [7.5, 8.0, 6.5])
aluno.cumprimentar()
let media = aluno.calcularMedia()
print("A média do aluno é \(media).")

let professor = Professor(nome: "Maria", idade: 35, disciplina: "Matemática")
professor.cumprimentar()
professor.ministrarAula()
```

Neste código, temos três classes: Pessoa, Aluno e Professor. A classe Pessoa é a classe base, com as propriedades nome e idade, além de um método cumprimentar() que imprime uma mensagem com o nome e a idade da pessoa.

A classe Aluno herda de Pessoa e adiciona as propriedades matricula (número de matrícula do aluno) e notas (um array de notas). Ela também possui um método calcularMedia() que calcula a média das notas do aluno e um método cumprimentar() que sobrescreve o método da classe base para adicionar informações específicas do aluno.

A classe Professor também herda de Pessoa e adiciona a propriedade disciplina (a disciplina que o professor ministra). Ela possui um método ministrarAula() que imprime uma mensagem sobre a disciplina que será estudada e um método cumprimentar() que sobrescreve o método da classe base para adicionar informações específicas do professor.

No final do código, criamos uma instância de Aluno e uma instância de Professor, e demonstramos a utilização dos métodos e propriedades das classes.