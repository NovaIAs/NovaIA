Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language), explicado em português:

```
class Aluno {
    nome : String <- ""
    idade : Int <- 0
    cursos : ListOfCurso <- nil

    met initialize(nome : String, idade : Int) : SELF_TYPE {
        {
            self.nome <- nome
            self.idade <- idade
            self.cursos <- new ListOfCurso
            self
        }
    }

    met adicionarCurso(curso : Curso) : Bool {
        {
            if not self.cursos.contains(curso) then
                self.cursos.add(curso)
                true
            else
                false
            fi
        }
    }

    met removerCurso(curso : Curso) : Bool {
        {
            if self.cursos.contains(curso) then
                self.cursos.remove(curso)
                true
            else
                false
            fi
        }
    }
}

class Curso {
    nome : String <- ""
    creditos : Int <- 0

    met initialize(nome : String, creditos : Int) : SELF_TYPE {
        {
            self.nome <- nome
            self.creditos <- creditos
            self
        }
    }
}

class ListOfCurso {
    cursos : ListOfCursoItem <- nil

    met initialize() : SELF_TYPE {
        {
            self.cursos <- new ListOfCursoItem
            self
        }
    }

    met add(curso : Curso) : Bool {
        {
            if not self.cursos.contains(curso) then
                self.cursos.add(new ListOfCursoItem(curso))
                true
            else
                false
            fi
        }
    }

    met remove(curso : Curso) : Bool {
        {
            if self.cursos.contains(curso) then
                self.cursos.remove(curso)
                true
            else
                false
            fi
        }
    }

    met contains(curso : Curso) : Bool {
        {
            let item : ListOfCursoItem <- self.cursos
            while item.next? loop
                if item.curso = curso then
                    return true
                else
                    item <- item.next
                fi
            pool
            false
        }
    }
}

class ListOfCursoItem {
    curso : Curso <- void
    next : ListOfCursoItem <- nil

    met initialize(curso : Curso) : SELF_TYPE {
        {
            self.curso <- curso
            self.next <- nil
            self
        }
    }
}
```

Este código implementa três classes em COOL: `Aluno`, `Curso` e `ListOfCurso`. A classe `Aluno` representa um aluno com nome, idade e uma lista de cursos. A classe `Curso` representa um curso com nome e número de créditos. A classe `ListOfCurso` é uma lista encadeada de cursos.

A classe `Aluno` possui métodos para adicionar e remover cursos da lista, bem como um método de inicialização para definir o nome e a idade do aluno. A classe `Curso` possui um método de inicialização para definir o nome e o número de créditos do curso. A classe `ListOfCurso` tem métodos para adicionar e remover cursos da lista, bem como um método para verificar se um curso está presente na lista. A classe `ListOfCursoItem` é uma classe auxiliar que representa um item da lista encadeada.

Este código mostra a implementação de estruturas de dados e lógica de negócios básica relacionada a alunos e cursos. Ele demonstra o uso de classes, herança, métodos, atributos e estruturas de controle em COOL.

Espero que isso tenha lhe dado uma ideia de como criar um código complexo em COOL!