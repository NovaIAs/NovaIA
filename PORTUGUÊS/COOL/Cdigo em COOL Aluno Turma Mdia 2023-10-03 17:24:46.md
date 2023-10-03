Claro! Aqui está um exemplo de um código complexo em COOL (Classroom Object-Oriented Language), escrito em português, com uma explicação detalhada:

```
class Aluno {
   atributo nome: String;
   atributo idade: Inteiro;
   atributo notas: Lista de Reais;
   
   metodo inicializar(nomeAluno: String, idadeAluno: Inteiro) {
      self.nome <- nomeAluno;
      self.idade <- idadeAluno;
      self.notas <- nova Lista de Reais;
   }
   
   metodo adicionarNota(nota: Real) {
      self.notas.adicionar(nota);
   }
   
   metodo calcularMedia(): Real {
      se self.notas.vazia() entao
         retornar 0.0;
      senao
         retornar self.notas.somar() / self.notas.tamanho();
   }
}

class Turma {
   atributo alunos: Lista de Aluno;
   
   metodo inicializar() {
      self.alunos <- nova Lista de Aluno;
   }
   
   metodo adicionarAluno(aluno: Aluno) {
      self.alunos.adicionar(aluno);
   }
   
   metodo calcularMediaTurma(): Real {
      se self.alunos.vazia() entao
         retornar 0.0;
      senao
         somatorio: Real <- 0.0;
         para cada aluno em self.alunos faca
            somatorio <- somatorio + aluno.calcularMedia();
         fim para
         retornar somatorio / self.alunos.tamanho();
   }
}

metodo principal() {
   turma: Turma <- nova Turma;
   turma.inicializar();
   
   aluno1: Aluno <- novo Aluno;
   aluno1.inicializar("João", 20);
   aluno1.adicionarNota(8.5);
   aluno1.adicionarNota(7.0);
   aluno1.adicionarNota(9.0);
   
   aluno2: Aluno <- novo Aluno;
   aluno2.inicializar("Maria", 19);
   aluno2.adicionarNota(6.5);
   aluno2.adicionarNota(8.0);
   aluno2.adicionarNota(7.5);
   
   turma.adicionarAluno(aluno1);
   turma.adicionarAluno(aluno2);
   
   mediaTurma: Real <- turma.calcularMediaTurma();
   
   escrever("A média da turma é: ", mediaTurma);
}
```

Neste código, temos duas classes: "Aluno" e "Turma". A classe "Aluno" possui os atributos "nome" (do tipo String), "idade" (do tipo Inteiro) e "notas" (uma Lista de Reais). Ela também possui os métodos "inicializar" (para definir os valores dos atributos), "adicionarNota" (para adicionar uma nova nota à lista de notas do aluno) e "calcularMedia" (para calcular a média das notas do aluno).

A classe "Turma" possui o atributo "alunos" (uma Lista de Aluno). Ela possui os métodos "inicializar" (para inicializar a lista de alunos), "adicionarAluno" (para adicionar um aluno à turma) e "calcularMediaTurma" (para calcular a média das médias de cada aluno na turma).

No método principal, criamos uma instância da classe "Turma" e inicializamos a turma. Em seguida, criamos duas instâncias da classe "Aluno", inicializamos cada aluno com seu nome e idade, adicionamos notas para cada aluno e os adicionamos à turma.

Finalmente, calculamos a média da turma usando o método "calcularMediaTurma" da classe "Turma" e a exibimos na tela usando a função "escrever".