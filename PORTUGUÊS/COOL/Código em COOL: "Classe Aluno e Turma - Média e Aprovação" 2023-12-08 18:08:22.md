Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language):

```
class Aluno {

  atributo nome: String;
  atributo idade: Int;
  atributo notas: Array<Int>;
  
  inicializar(n: String, i: Int, nt: Array<Int>) : Object {
    nome <- n;
    idade <- i;
    notas <- nt;
    retorne self;
  };
  
  metodo calcularMedia() : Int {
    soma: Int <- 0;
    para cada nota em notas faça
      soma <- soma + nota;
    fim;
    
    media: Int <- soma / notas.tamanho();
    retorne media;
  };
  
  metodo isAprovado() : Bool {
    media: Int <- calcularMedia();
    
    se media >= 6 então
      retorne true;
    senão
      retorne false;
    fim;
  };
  
}; // fim da classe Aluno

classe Turma {

  atributo alunos: Array<Aluno>;
  
  inicializar(al: Array<Aluno>) : Object {
    alunos <- al;
    retorne self;
  };
  
  metodo calcularMediaTurma() : Int {
    somaMedias: Int <- 0;
    quantidadeAlunos: Int <- alunos.tamanho();
    
    para cada aluno em alunos faça
      somaMedias <- somaMedias + aluno.calcularMedia();
    fim;
    
    mediaTurma: Int <- somaMedias / quantidadeAlunos;
    retorne mediaTurma;
  };
  
  metodo imprimirAprovados() : Object {
    para cada aluno em alunos faça
      se aluno.isAprovado() então
        escreva(aluno.nome + " está aprovado!");
      fim;
    fim;
    
    retorne self;
  };
  
}; // fim da classe Turma


// Exemplo de uso das classes Aluno e Turma:

aluno1: Aluno <- Aluno(inicializar("João", 18, [7, 8, 6]));
aluno2: Aluno <- Aluno(inicializar("Maria", 17, [9, 9, 9]));
aluno3: Aluno <- Aluno(inicializar("Pedro", 16, [5, 6, 7]));
aluno4: Aluno <- Aluno(inicializar("Ana", 19, [8, 7, 8]));

turma: Turma <- Turma(inicializar([aluno1, aluno2, aluno3, aluno4]));

escreva("Média da Turma: ", turma.calcularMediaTurma());
turma.imprimirAprovados();
```

Neste código, temos duas classes: Aluno e Turma. 

A classe Aluno possui os atributos nome (String), idade (Int) e notas (Array<Int>). O método `inicializar` atribui os valores iniciais aos atributos do aluno. O método `calcularMedia` calcula a média das notas do aluno e o método `isAprovado` verifica se o aluno está aprovado ou não, considerando a média mínima de 6.

A classe Turma possui o atributo alunos (Array<Aluno>). O método `inicializar` atribui os valores iniciais aos alunos da turma. O método `calcularMediaTurma` calcula a média da turma, somando as médias de todos os alunos e dividindo pelo número de alunos. O método `imprimirAprovados` imprime na tela os alunos que estão aprovados.

A parte final do código cria instâncias das classes Aluno e Turma, atribuindo valores iniciais aos atributos dos objetos. Em seguida, são chamados os métodos da classe Turma para calcular a média da turma e imprimir os alunos aprovados.

Esse código é apenas um exemplo complexo em COOL e pode ser adaptado e expandido de acordo com as necessidades do desenvolvedor.