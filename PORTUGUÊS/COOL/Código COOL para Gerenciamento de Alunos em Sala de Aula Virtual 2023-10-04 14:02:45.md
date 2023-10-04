Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de alunos em uma sala de aula virtual:

```cool
class Aluno {
  attr nome : String;
  attr matricula : Int;
  attr notas : Array<Int>;

  method init(n : String, m : Int) : Aluno {
    nome <- n;
    matricula <- m;
    notas <- new Array;
    self;
  };

  method adicionarNota(nota : Int) : Object {
    notas.add(nota);
    self;
  };

  method calcularMedia() : Int {
    var soma : Int;
    soma <- 0;
    let i : Int <- 0;
    while (i < notas.length) loop
      soma <- soma + notas[i];
      i <- i + 1;
    pool;
    soma / notas.length;
  };

  method imprimirDados() : Object {
    out_string("Nome: " + nome + "\n");
    out_string("Matrícula: " + matricula + "\n");
    out_string("Notas: ");
    let i : Int <- 0;
    while (i < notas.length) loop
      out_int(notas[i]);
      if i < notas.length - 1 then
        out_string(", ");
      fi;
      i <- i + 1;
    pool;
    out_string("\n");
    self;
  };
};

class SalaDeAula {
  attr alunos : Array<Aluno>;

  method init() : SalaDeAula {
    alunos <- new Array;
    self;
  };

  method adicionarAluno(a : Aluno) : Object {
    alunos.add(a);
    self;
  };

  method removerAluno(matricula : Int) : Object {
    let i : Int <- 0;
    while (i < alunos.length) loop
      if alunos[i].matricula = matricula then
        alunos.remove(i);
        i <- alunos.length;
      fi;
      i <- i + 1;
    pool;
    self;
  };

  method calcularMediaTurma() : Int {
    var soma : Int;
    soma <- 0;
    let i : Int <- 0;
    while (i < alunos.length) loop
      soma <- soma + alunos[i].calcularMedia();
      i <- i + 1;
    pool;
    soma / alunos.length;
  };

  method imprimirDadosTurma() : Object {
    out_string("----- Dados da Turma -----\n");
    let i : Int <- 0;
    while (i < alunos.length) loop
      alunos[i].imprimirDados();
      i <- i + 1;
    pool;
    out_string("--------------------------\n");
    self;
  };
};

class Main {
  inherits IO;

  method main() : Object {
    let turma : SalaDeAula <- new SalaDeAula;
    let aluno1 : Aluno <- new Aluno.init("João", 123);
    aluno1.adicionarNota(8);
    aluno1.adicionarNota(7);
    let aluno2 : Aluno <- new Aluno.init("Maria", 456);
    aluno2.adicionarNota(9);
    aluno2.adicionarNota(10);
    turma.adicionarAluno(aluno1);
    turma.adicionarAluno(aluno2);
    turma.imprimirDadosTurma();
    out_string("Média da turma: ");
    out_int(turma.calcularMediaTurma());
    out_string("\n");
    self;
  };
};
```

Neste código, temos três classes principais: `Aluno`, `SalaDeAula` e `Main`. A classe `Aluno` representa um aluno em uma sala de aula virtual e possui atributos como nome, matrícula e notas. Essa classe possui métodos para inicialização, adição de notas, cálculo da média e impressão dos dados do aluno.

A classe `SalaDeAula` representa a sala de aula virtual e possui um atributo que é um array de alunos. Essa classe possui métodos para adicionar e remover alunos da sala, calcular a média da turma e imprimir os dados de todos os alunos.

Por fim, a classe `Main` é a classe principal do programa, que estende a classe `IO` e possui o método `main`. Neste método, criamos uma instância da classe `SalaDeAula`, instâncias de dois alunos, adicionamos esses alunos à sala de aula, imprimimos os dados da turma e calculamos a média da turma.

Esse código é apenas um exemplo de implementação de um sistema de gerenciamento de alunos em uma sala de aula virtual em COOL. É possível adicionar mais funcionalidades e aprimoramentos a partir desse ponto de partida.