Claro! Aqui está um código em COOL (Classroom Object-Oriented Language) que permite ao usuário criar e gerenciar uma sala de aula virtual, com alunos, professores e cursos. O código foi desenvolvido em português e é bastante complexo:

```coolidge
class Pessoa {
    atributo nome : String <- ""

    metodo inicialize(n : String) : SELF_TYPE {
        nome <- n
        return self
    }

    metodo obterNome() : String {
        return nome
    }
}

class Aluno herda Pessoa {
    atributo cursos : Lista(Curso) <- new Lista(Curso).inicialize()
    atributo professores : Lista(Professor) <- new Lista(Professor).inicialize()

    metodo adicionarCurso(curso : Curso) : AUTO_TYPE {
        cursos.adicionar(curso)
        return self
    }

    metodo adicionarProfessor(professor : Professor) : AUTO_TYPE {
        professores.adicionar(professor)
        return self
    }

    metodo listarCursos() : AUTO_TYPE {
        se cursos.tamanho() = 0 entao
            imprimir("O aluno ", obterNome(), " não está matriculado em nenhum curso.")
        senao
            imprimir("O aluno ", obterNome(), " está matriculado nos seguintes cursos:")
            para cada curso em cursos.fornecerElementos() faca
                imprimir("- ", curso.obterNome())
            fim
        fim

        retornar self
    }

    metodo listarProfessores() : AUTO_TYPE {
        se professores.tamanho() = 0 entao
            imprimir("O aluno ", obterNome(), " não possui nenhum professor associado.")
        senao
            imprimir("O aluno ", obterNome(), " possui os seguintes professores:")
            para cada professor em professores.fornecerElementos() faca
                imprimir("- ", professor.obterNome())
            fim
        fim

        retornar self
    }
}

class Professor herda Pessoa {
    atributo cursos : Lista(Curso) <- new Lista(Curso).inicialize()
    atributo alunos : Lista(Aluno) <- new Lista(Aluno).inicialize()

    metodo adicionarCurso(curso : Curso) : AUTO_TYPE {
        cursos.adicionar(curso)
        return self
    }

    metodo adicionarAluno(aluno : Aluno) : AUTO_TYPE {
        alunos.adicionar(aluno)
        return self
    }

    metodo listarCursos() : AUTO_TYPE {
        se cursos.tamanho() = 0 entao
            imprimir("O professor ", obterNome(), " não está associado a nenhum curso.")
        senao
            imprimir("O professor ", obterNome(), " está associado aos seguintes cursos:")
            para cada curso em cursos.fornecerElementos() faca
                imprimir("- ", curso.obterNome())
            fim
        fim

        retornar self
    }

    metodo listarAlunos() : AUTO_TYPE {
        se alunos.tamanho() = 0 entao
            imprimir("O professor ", obterNome(), " não possui nenhum aluno associado.")
        senao
            imprimir("O professor ", obterNome(), " possui os seguintes alunos:")
            para cada aluno em alunos.fornecerElementos() faca
                imprimir("- ", aluno.obterNome())
            fim
        fim

        retornar self
    }
}

class Curso {
    atributo nome : String <- ""
    atributo professor : Professor <- void

    metodo inicialize(n : String, p : Professor) : SELF_TYPE {
        nome <- n
        professor <- p
        return self
    }

    metodo obterNome() : String {
        return nome
    }

    metodo obterProfessor() : Professor {
        return professor
    }
}

class Lista(T) {
    atributo elementos : Array(T) <- new array of T
    atributo tamanhoLista : Int <- 0

    metodo inicialize() : SELF_TYPE {
        elementos <- new array of T
        tamanhoLista <- 0
        return self
    }

    metodo adicionar(elemento : T) : AUTO_TYPE {
        elementos[tamanhoLista] <- elemento
        tamanhoLista <- tamanhoLista + 1
        return self
    }

    metodo fornecerElementos() : Array(T) {
        return elementos
    }

    metodo tamanho() : Int {
        return tamanhoLista
    }
}

--- Programa Principal ---

let aluno1 : Aluno <- new Aluno.inicialize("João")
let aluno2 : Aluno <- new Aluno.inicialize("Maria")
let professor1 : Professor <- new Professor.inicialize("Pedro")
let professor2 : Professor <- new Professor.inicialize("Ana")
let curso1 : Curso <- new Curso.inicialize("Matemática", professor1)
let curso2 : Curso <- new Curso.inicialize("História", professor2)

aluno1.adicionarCurso(curso1)
aluno1.adicionarCurso(curso2)
aluno1.adicionarProfessor(professor1)
aluno1.listarCursos()
aluno1.listarProfessores()

professor1.adicionarCurso(curso1)
professor1.adicionarAluno(aluno1)
professor1.listarCursos()
professor1.listarAlunos()
```

Neste código, temos as classes `Pessoa`, `Aluno`, `Professor`, `Curso` e `Lista`. A classe `Pessoa` é a classe base para `Aluno` e `Professor`, contendo o atributo `nome` e os métodos `inicialize` e `obterNome`. As classes `Aluno` e `Professor` possuem atributos adicionais, como `cursos` e `professores` para `Aluno`, e `cursos` e `alunos` para `Professor`. Essas classes também possuem métodos para adicionar cursos e professores, bem como listar os cursos e professores associados.

A classe `Curso` possui atributos `nome` e `professor`, permitindo associar um professor a um curso. Ela também possui métodos para obter o nome e o professor do curso.

A classe `Lista` é uma implementação de uma lista genérica, que é usada para armazenar os cursos e os professores associados a um aluno, bem como os cursos e os alunos associados a um professor. Ela possui métodos para adicionar elementos, fornecer os elementos armazenados e obter o tamanho da lista.

No programa principal, criamos instâncias de alunos, professores e cursos, e realizamos diversas operações, como adicionar cursos e professores, listar os cursos e professores associados a um aluno, e listar os cursos e alunos associados a um professor.

Espero que isso tenha atendido às suas expectativas! Se você tiver mais alguma solicitação, por favor, me avise.