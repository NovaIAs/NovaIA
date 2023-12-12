Claro! Aqui está um exemplo de código em COOL (Classroom Object-Oriented Language), uma linguagem de programação de alto nível desenvolvida pela Universidade de Berkeley:

```
class Aluno {

  atributo nome : String <- "João";
  atributo idade : Inteiro <- 18;
  atributo notaFinal : Inteiro <- 0;

  metodo inicializar(nome : String, idade : Inteiro) : Nada {
    self.nome <- nome;
    self.idade <- idade;
  }

  metodo calcularNotaFinal(notas : Lista<Inteiro>) : Nada {
    var soma : Inteiro <- 0;
    var numNotas : Inteiro <- notas.tamanho();

    para cada nota em notas faça {
      soma <- soma + nota;
    }

    self.notaFinal <- soma / numNotas;
  }

  metodo exibirInformacoes() : Nada {
    console.escrever("Nome: " + self.nome);
    console.escrever("Idade: " + self.idade);
    console.escrever("Nota Final: " + self.notaFinal);
  }
}

classe Turma {

  atributo alunos : Lista<Aluno> <- nova Lista<Aluno>();

  metodo adicionarAluno(aluno : Aluno) : Nada {
    alunos.adicionar(aluno);
  }

  metodo calcularMediaTurma() : Inteiro {
    var soma : Inteiro <- 0;
    var numAlunos : Inteiro <- alunos.tamanho();

    para cada aluno em alunos faça {
      soma <- soma + aluno.notaFinal;
    }

    se numAlunos != 0 então
      retorne soma / numAlunos;
    senão
      retorne 0;
  }

  metodo exibirInformacoesTurma() : Nada {
    console.escrever("----- Informações da Turma -----");
    para cada aluno em alunos faça {
      aluno.exibirInformacoes();
      console.escrever("--------------------");
    }
    console.escrever("Média da Turma: " + self.calcularMediaTurma());
  }
}

metodo principal() : Nada {
  var turma : Turma <- nova Turma();

  var aluno1 : Aluno <- novo Aluno();
  aluno1.inicializar("Maria", 20);
  aluno1.calcularNotaFinal([75, 80, 85, 90]);
  turma.adicionarAluno(aluno1);

  var aluno2 : Aluno <- novo Aluno();
  aluno2.inicializar("Pedro", 19);
  aluno2.calcularNotaFinal([80, 85, 90, 95]);
  turma.adicionarAluno(aluno2);

  turma.exibirInformacoesTurma();
}
```

Neste código COOL, temos duas classes: `Aluno` e `Turma`. A classe `Aluno` possui atributos como nome, idade e nota final, bem como métodos para inicializar um aluno, calcular a nota final com base em uma lista de notas e exibir informações do aluno.

A classe `Turma` possui um atributo que é uma lista de alunos e métodos para adicionar alunos à turma, calcular a média da turma com base nas notas finais dos alunos e exibir informações da turma, incluindo as informações de cada aluno.

No método `principal`, criamos uma instância da classe `Turma`, criamos dois alunos, inicializamos seus atributos, calculamos suas notas finais e adicionamos os alunos à turma. Por fim, exibimos as informações da turma, incluindo a média da turma.