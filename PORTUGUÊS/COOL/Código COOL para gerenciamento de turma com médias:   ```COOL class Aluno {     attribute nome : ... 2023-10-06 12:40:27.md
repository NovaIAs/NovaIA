Claro! Aqui está um exemplo de um código complexo em COOL (Classroom Object-Oriented Language):

```
class Aluno {

    attribute nome : String
    attribute idade : Int
    attribute notas : List<Int>

    method init(nome: String, idade: Int, notas: List<Int>) : SELF_TYPE {
        self.nome <- nome
        self.idade <- idade
        self.notas <- notas
        self
    }

    method calcularMedia() : Float {
        var soma : Int <- 0
        var quantidade : Int <- 0

        for nota in self.notas loop
            soma <- soma + nota
            quantidade <- quantidade + 1
        pool

        (soma / quantidade).asFloat()
    }

    method imprimirInformacoes() : Object {
        out_string("Nome: " + self.nome + "\n")
        out_string("Idade: " + self.idade.toString() + "\n")
        out_string("Notas: ")

        for nota in self.notas loop
            out_string(nota.toString() + " ")
        pool

        out_string("\n")

        out_string("Média: " + self.calcularMedia().asString() + "\n")

        self
    }
}

class Turma {

    attribute alunos : List<Aluno>

    method init(alunos: List<Aluno>) : SELF_TYPE {
        self.alunos <- alunos
        self
    }

    method calcularMediaTurma() : Float {
        var soma : Int <- 0
        var quantidade : Int <- 0

        for aluno in self.alunos loop
            soma <- soma + aluno.calcularMedia().asInt()
            quantidade <- quantidade + 1
        pool

        (soma / quantidade).asFloat()
    }

    method exibirInformacoesTurma() : Object {
        out_string("Informações da Turma\n")
        out_string("=====================\n")

        for aluno in self.alunos loop
            out_string("\n")
            aluno.imprimirInformacoes()
        pool

        out_string("=====================\n")
        out_string("Média da Turma: " + self.calcularMediaTurma().asString() + "\n")

        self
    }
}

class Main {

    method main() : Object {
        var notasAluno1 : List<Int> <- new List<Int>.empty()
        notasAluno1.insert_last(8)
        notasAluno1.insert_last(9)
        notasAluno1.insert_last(7)

        var aluno1 : Aluno <- new Aluno.init("João", 18, notasAluno1)

        var notasAluno2 : List<Int> <- new List<Int>.empty()
        notasAluno2.insert_last(6)
        notasAluno2.insert_last(7)
        notasAluno2.insert_last(8)

        var aluno2 : Aluno <- new Aluno.init("Maria", 17, notasAluno2)

        var notasAluno3 : List<Int> <- new List<Int>.empty()
        notasAluno3.insert_last(9)
        notasAluno3.insert_last(10)
        notasAluno3.insert_last(9)

        var aluno3 : Aluno <- new Aluno.init("Pedro", 16, notasAluno3)

        var alunos : List<Aluno> <- new List<Aluno>.empty()
        alunos.insert_last(aluno1)
        alunos.insert_last(aluno2)
        alunos.insert_last(aluno3)

        var turma : Turma <- new Turma.init(alunos)

        turma.exibirInformacoesTurma()
    }
}
```

Este código em COOL simula um sistema de gerenciamento de uma turma de alunos. A classe `Aluno` define as informações de um aluno, como nome, idade e suas notas em uma lista. Ela possui métodos para calcular a média das notas e para imprimir as informações do aluno.

A classe `Turma` representa uma turma de alunos e possui uma lista de objetos `Aluno`. Ela possui métodos para calcular a média da turma e para exibir as informações de todos os alunos.

A classe `Main` contém o método `main()`, que é o ponto de entrada do programa. Neste método, são criados três objetos `Aluno` com suas respectivas notas, e em seguida é criado um objeto `Turma` com os alunos criados. Por fim, o método `exibirInformacoesTurma()` é chamado para exibir as informações da turma.

Espero que este código complexo em COOL atenda às suas expectativas!