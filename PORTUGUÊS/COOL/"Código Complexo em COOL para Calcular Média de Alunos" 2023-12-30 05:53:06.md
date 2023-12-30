Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language), explicado em detalhes:

```
class Aluno {

   atributo nome : String;
   atributo idade : Int;
   atributo notas : Lista[Int];

   metodo adicionarNota(nota : Int) : Object {
      self.notas.insere(nota);
      return self;
   }

   metodo calcularMedia() : Float {
      var soma : Int <- 0;
      var quantidade : Int <- self.notas.comprimento();
      
      self.notas.iterar( n : Int ) : Object {
         soma <- soma + n;
      };
      
      return (soma / quantidade).paraFloat();
   }

   metodo imprimirRelatorio() : Object {
      Console.outString("Nome: " + self.nome + "\n");
      Console.outString("Idade: " + self.idade.toString() + "\n");
      Console.outString("Notas: " + self.notas.toString() + "\n");
      Console.outString("Média: " + self.calcularMedia().toString() + "\n");
      
      return self;
   }
}

class Main {
   inicio : Object {
      var aluno : Aluno <- novo Aluno;
      
      aluno.nome <- "João";
      aluno.idade <- 20;
      
      aluno.adicionarNota(8);
      aluno.adicionarNota(7);
      aluno.adicionarNota(9);
      
      aluno.imprimirRelatorio();
      
      return 0;
   }
}
```

Neste exemplo, temos duas classes: `Aluno` e `Main`. A classe `Aluno` contém três atributos: `nome`, `idade` e `notas`. O atributo `notas` é uma lista de números inteiros.

A classe `Aluno` possui três métodos: `adicionarNota`, `calcularMedia` e `imprimirRelatorio`. O método `adicionarNota` recebe uma nota e a insere na lista de notas do aluno. O método `calcularMedia` realiza a soma das notas e retorna a média. O método `imprimirRelatorio` exibe na saída padrão o nome, idade, notas e média do aluno.

A classe `Main` é a classe principal, onde o programa é iniciado. No método `inicio`, criamos um objeto `aluno`, definimos seu nome e idade, adicionamos notas e, por fim, chamamos o método `imprimirRelatorio` para exibir as informações do aluno.

Espero que este código atenda às suas expectativas!