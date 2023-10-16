Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de uma escola:

```
class Escola {

    var alunos: Array<Aluno>
    var professores: Array<Professor>
    var disciplinas: Array<Disciplina>
    var turmas: Array<Turma>

    # Método construtor
    construtor() : Object {
        alunos <- new Array<Aluno>(0)
        professores <- new Array<Professor>(0)
        disciplinas <- new Array<Disciplina>(0)
        turmas <- new Array<Turma>(0)
        retorna self
    }

    # Métodos para adicionar e remover alunos, professores, disciplinas e turmas
    metodo adicionarAluno(aluno: Aluno) : Object {
        alunos.insert(aluno, alunos.length)
        retorna self
    }

    metodo removerAluno(aluno: Aluno) : Object {
        alunos.remove(aluno)
        retorna self
    }

    metodo adicionarProfessor(professor: Professor) : Object {
        professores.insert(professor, professores.length)
        retorna self
    }

    metodo removerProfessor(professor: Professor) : Object {
        professores.remove(professor)
        retorna self
    }

    metodo adicionarDisciplina(disciplina: Disciplina) : Object {
        disciplinas.insert(disciplina, disciplinas.length)
        retorna self
    }

    metodo removerDisciplina(disciplina: Disciplina) : Object {
        disciplinas.remove(disciplina)
        retorna self
    }

    metodo adicionarTurma(turma: Turma) : Object {
        turmas.insert(turma, turmas.length)
        retorna self
    }

    metodo removerTurma(turma: Turma) : Object {
        turmas.remove(turma)
        retorna self
    }

    # Método para obter a lista de alunos matriculados em uma disciplina
    metodo obterAlunosPorDisciplina(disciplina: Disciplina) : Array<Aluno> {
        var alunosPorDisciplina: Array<Aluno> <- new Array<Aluno>(0)
        para aluno em alunos {
            se aluno.estaMatriculado(disciplina) {
                alunosPorDisciplina.insert(aluno, alunosPorDisciplina.length)
            }
        }
        retorna alunosPorDisciplina
    }

    # Outros métodos para manipulação e consulta dos dados da escola
    ...
}

class Aluno {

    var nome: String
    var matricula: Int
    var disciplinasMatriculadas: Array<Disciplina>

    # Método construtor
    construtor(nome: String, matricula: Int) : Object {
        self.nome <- nome
        self.matricula <- matricula
        disciplinasMatriculadas <- new Array<Disciplina>(0)
        retorna self
    }

    metodo matricularDisciplina(disciplina: Disciplina) : Object {
        disciplinasMatriculadas.insert(disciplina, disciplinasMatriculadas.length)
        retorna self
    }

    metodo desmatricularDisciplina(disciplina: Disciplina) : Object {
        disciplinasMatriculadas.remove(disciplina)
        retorna self
    }

    metodo estaMatriculado(disciplina: Disciplina) : Bool {
        retorna disciplinasMatriculadas.contains(disciplina)
    }

    # Outros métodos para manipulação e consulta dos dados do aluno
    ...
}

class Professor {

    var nome: String
    var disciplinasLecionadas: Array<Disciplina>

    # Método construtor
    construtor(nome: String) : Object {
        self.nome <- nome
        disciplinasLecionadas <- new Array<Disciplina>(0)
        retorna self
    }

    metodo lecionarDisciplina(disciplina: Disciplina) : Object {
        disciplinasLecionadas.insert(disciplina, disciplinasLecionadas.length)
        retorna self
    }

    metodo deixarDisciplina(disciplina: Disciplina) : Object {
        disciplinasLecionadas.remove(disciplina)
        retorna self
    }

    # Outros métodos para manipulação e consulta dos dados do professor
    ...
}

class Disciplina {

    var nome: String

    # Método construtor
    construtor(nome: String) : Object {
        self.nome <- nome
        retorna self
    }

    # Outros métodos para manipulação e consulta dos dados da disciplina
    ...
}

class Turma {

    var nome: String
    var alunosMatriculados: Array<Aluno>
    var professorResponsavel: Professor
    var disciplina: Disciplina

    # Método construtor
    construtor(nome: String, professor: Professor, disciplina: Disciplina) : Object {
        self.nome <- nome
        alunosMatriculados <- new Array<Aluno>(0)
        professorResponsavel <- professor
        self.disciplina <- disciplina
        retorna self
    }

    metodo adicionarAluno(aluno: Aluno) : Object {
        alunosMatriculados.insert(aluno, alunosMatriculados.length)
        retorna self
    }

    metodo removerAluno(aluno: Aluno) : Object {
        alunosMatriculados.remove(aluno)
        retorna self
    }

    # Outros métodos para manipulação e consulta dos dados da turma
    ...
}
```

Nesse código em COOL, temos quatro classes principais: `Escola`, `Aluno`, `Professor` e `Disciplina`, além da classe auxiliar `Turma`. A classe `Escola` é responsável por gerenciar os alunos, professores, disciplinas e turmas da escola. Ela possui métodos para adicionar e remover esses elementos, bem como métodos para consultar informações, como a lista de alunos matriculados em uma disciplina.

A classe `Aluno` representa um aluno da escola, com atributos como nome, matrícula e disciplinas matriculadas. Ela possui métodos para matricular e desmatricular disciplinas, bem como para verificar se está matriculado em uma determinada disciplina.

A classe `Professor` representa um professor da escola, com atributos como nome e disciplinas lecionadas. Ela possui métodos para lecionar e deixar de lecionar disciplinas.

A classe `Disciplina` representa uma disciplina da escola, com atributos como nome. Ela não possui muitos métodos nesse exemplo, mas poderia ter métodos para manipular e consultar os dados da disciplina.

A classe `Turma` representa uma turma da escola, com atributos como nome, alunos matriculados, professor responsável e disciplina. Ela possui métodos para adicionar e remover alunos da turma.

Esse código em COOL implementa um sistema básico de gerenciamento de uma escola, permitindo adicionar e remover alunos, professores, disciplinas e turmas, além de realizar consultas como obter a lista de alunos matriculados em uma disciplina.