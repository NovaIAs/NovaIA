Claro! Aqui está um exemplo de código em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de notas para uma sala de aula.

```
class Aluno {

    atributo nome : String;
    atributo notas : List<Float>;

    metodo init(nome: String) : SELF_TYPE {
        self.nome <- nome;
        self.notas <- nil;
        return self;
    }

    metodo adicionarNota(nota: Float) : SELF_TYPE {
        se self.notas = nil entao
            self.notas <- cons(nota, nil);
        senao
            self.notas <- cons(nota, self.notas);
        fim;
        return self;
    }

    metodo calcularMedia() : Float {
        se self.notas = nil entao
            return 0.0;
        senao
            soma : Float <- 0.0;
            contador : Int <- 0;
            enquanto self.notas != nil faca
                soma <- soma + head(self.notas);
                self.notas <- tail(self.notas);
                contador <- contador + 1;
            fim;
            return soma / contador;
        fim;
    }

}

class SalaDeAula {

    atributo alunos : List<Aluno>;

    metodo init() : SELF_TYPE {
        self.alunos <- nil;
        return self;
    }

    metodo adicionarAluno(aluno: Aluno) : SELF_TYPE {
        self.alunos <- cons(aluno, self.alunos);
        return self;
    }

    metodo calcularMediaTurma() : Float {
        soma : Float <- 0.0;
        contador : Int <- 0;
        para cada aluno em self.alunos faca
            soma <- soma + aluno.calcularMedia();
            contador <- contador + 1;
        fim;
        return soma / contador;
    }

}

metodo principal() : Object {
    salaDeAula : SalaDeAula <- new SalaDeAula();
    
    aluno1 : Aluno <- new Aluno().init("João");
    aluno1.adicionarNota(8.5);
    aluno1.adicionarNota(9.0);
    salaDeAula.adicionarAluno(aluno1);
    
    aluno2 : Aluno <- new Aluno().init("Maria");
    aluno2.adicionarNota(7.0);
    aluno2.adicionarNota(6.5);
    salaDeAula.adicionarAluno(aluno2);
    
    mediaTurma : Float <- salaDeAula.calcularMediaTurma();
    print("A média da turma é: ", mediaTurma);
    
    retorne 0;
}
```

Neste código COOL, temos a definição das classes `Aluno` e `SalaDeAula`. A classe `Aluno` possui os atributos `nome` (que armazena o nome do aluno) e `notas` (que armazena uma lista de notas). Essa classe possui os métodos `init` (que inicializa o objeto com um nome), `adicionarNota` (que adiciona uma nota à lista de notas do aluno) e `calcularMedia` (que calcula a média das notas do aluno).

A classe `SalaDeAula` representa uma sala de aula e possui o atributo `alunos` (que armazena uma lista de alunos). Essa classe possui os métodos `init` (que inicializa a sala de aula), `adicionarAluno` (que adiciona um aluno à lista de alunos) e `calcularMediaTurma` (que calcula a média da turma, considerando as médias individuais de cada aluno).

No método `principal`, criamos uma instância da classe `SalaDeAula` e instâncias das classes `Aluno` para representar os alunos. Adicionamos notas aos alunos e, em seguida, calculamos e exibimos a média da turma.

Espero que esse código complexo em COOL atenda às suas expectativas!