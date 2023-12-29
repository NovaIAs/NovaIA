import Foundation

// Classe Pessoa
class Pessoa {
    var nome: String
    var idade: Int
    
    init(nome: String, idade: Int) {
        self.nome = nome
        self.idade = idade
    }
    
    func apresentar() {
        print("Olá, meu nome é \(nome) e tenho \(idade) anos.")
    }
}

// Classe Aluno
class Aluno: Pessoa {
    var matricula: Int
    
    init(nome: String, idade: Int, matricula: Int) {
        self.matricula = matricula
        super.init(nome: nome, idade: idade)
    }
    
    override func apresentar() {
        print("Olá, meu nome é \(nome), tenho \(idade) anos e minha matrícula é \(matricula).")
    }
}

// Classe Professor
class Professor: Pessoa {
    var disciplina: String
    
    init(nome: String, idade: Int, disciplina: String) {
        self.disciplina = disciplina
        super.init(nome: nome, idade: idade)
    }
    
    override func apresentar() {
        print("Olá, meu nome é \(nome), tenho \(idade) anos e leciono a disciplina de \(disciplina).")
    }
}

// Classe Escola
class Escola {
    var nome: String
    var alunos: [Aluno]
    var professores: [Professor]
    
    init(nome: String) {
        self.nome = nome
        self.alunos = []
        self.professores = []
    }
    
    func adicionarAluno(aluno: Aluno) {
        alunos.append(aluno)
    }
    
    func removerAluno(matricula: Int) {
        alunos = alunos.filter { $0.matricula != matricula }
    }
    
    func adicionarProfessor(professor: Professor) {
        professores.append(professor)
    }
    
    func removerProfessor(nome: String) {
        professores = professores.filter { $0.nome != nome }
    }
    
    func listarAlunos() {
        if alunos.isEmpty {
            print("Não há alunos cadastrados.")
        } else {
            print("Alunos cadastrados na escola \(nome):")
            for aluno in alunos {
                aluno.apresentar()
            }
        }
    }
    
    func listarProfessores() {
        if professores.isEmpty {
            print("Não há professores cadastrados.")
        } else {
            print("Professores cadastrados na escola \(nome):")
            for professor in professores {
                professor.apresentar()
            }
        }
    }
}

// Testando o código
let escola = Escola(nome: "Escola ABC")

let aluno1 = Aluno(nome: "João", idade: 18, matricula: 123)
let aluno2 = Aluno(nome: "Maria", idade: 17, matricula: 124)
let aluno3 = Aluno(nome: "Pedro", idade: 16, matricula: 125)

escola.adicionarAluno(aluno: aluno1)
escola.adicionarAluno(aluno: aluno2)
escola.adicionarAluno(aluno: aluno3)

let professor1 = Professor(nome: "Carlos", idade: 35, disciplina: "Matemática")
let professor2 = Professor(nome: "Ana", idade: 40, disciplina: "Português")

escola.adicionarProfessor(professor: professor1)
escola.adicionarProfessor(professor: professor2)

escola.listarAlunos()
escola.listarProfessores()