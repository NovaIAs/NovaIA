Claro! Aqui está um exemplo de código em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de alunos em uma escola:

```
class Escola {

   // Classe para representar um aluno
   class Aluno {
      name : String <- "John Doe"; // Nome do aluno
      idade : Int <- 0; // Idade do aluno
      notas : List<Int> <- []; // Lista de notas do aluno

      // Método para adicionar uma nota à lista de notas
      method adicionarNota(nota : Int) : Object {
         notas.add(nota);
      };

      // Método para calcular a média das notas do aluno
      method calcularMedia() : Int {
         var soma : Int <- 0;
         for nota in notas loop
            soma <- soma + nota;
         end;
         return soma / notas.length;
      };

      // Método para verificar se o aluno está aprovado
      method verificarAprovacao() : String {
         var media : Int <- calcularMedia();
         if media >= 60 then
            return "Aprovado";
         else
            return "Reprovado";
         end;
      };
   };

   alunos : List<Aluno> <- []; // Lista de alunos da escola

   // Método para adicionar um aluno à lista de alunos
   method adicionarAluno(aluno : Aluno) : Object {
      alunos.add(aluno);
   };

   // Método para encontrar um aluno pelo nome
   method encontrarAluno(nome : String) : Aluno {
      for aluno in alunos loop
         if aluno.name = nome then
            return aluno;
         end;
      end;
      return null;
   };
};

// Exemplo de uso do sistema de gerenciamento de alunos
var escola : Escola <- Escola;

// Criar alunos
var aluno1 : Escola.Aluno <- escola.Aluno;
aluno1.name <- "João";
aluno1.idade <- 16;
aluno1.adicionarNota(80);
aluno1.adicionarNota(75);

var aluno2 : Escola.Aluno <- escola.Aluno;
aluno2.name <- "Maria";
aluno2.idade <- 17;
aluno2.adicionarNota(90);
aluno2.adicionarNota(85);

// Adicionar alunos à escola
escola.adicionarAluno(aluno1);
escola.adicionarAluno(aluno2);

// Encontrar aluno pelo nome
var alunoEncontrado : Escola.Aluno <- escola.encontrarAluno("João");

// Imprimir informações do aluno encontrado
if alunoEncontrado /= null then
   print("Nome: " + alunoEncontrado.name);
   print("Idade: " + alunoEncontrado.idade);
   print("Notas: " + alunoEncontrado.notas);
   print("Média: " + alunoEncontrado.calcularMedia());
   print("Situação: " + alunoEncontrado.verificarAprovacao());
else
   print("Aluno não encontrado!");
end;
```

Neste código, a classe `Escola` possui uma classe interna `Aluno` que representa um aluno. Cada aluno tem um nome, idade e uma lista de notas. Os métodos `adicionarNota`, `calcularMedia` e `verificarAprovacao` são utilizados para adicionar notas, calcular a média das notas e verificar se o aluno está aprovado, respectivamente.

A classe `Escola` também possui uma lista de alunos e os métodos `adicionarAluno` e `encontrarAluno` são utilizados para adicionar um aluno à lista e encontrar um aluno pelo nome, respectivamente.

No exemplo de uso do sistema, são criados dois alunos, adicionados à escola e, em seguida, é encontrado um aluno pelo nome "João". Suas informações são impressas, incluindo o nome, idade, notas, média e situação (aprovado ou reprovado).

Espero que este código atenda às suas expectativas!