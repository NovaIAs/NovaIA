Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de alunos em uma escola.

```cool
class Aluno {
  atributo nome : String;
  atributo idade : Int;
  atributo notas : List<Int>;
  
  metodo init(nome : String, idade : Int) : SELF_TYPE {
    {
      self.nome <- nome;
      self.idade <- idade;
      self.notas <- new List<Int>;
      self;
    }
  }
  
  metodo adicionarNota(nota : Int) : SELF_TYPE {
    {
      self.notas.append(nota);
      self;
    }
  }
  
  metodo calcularMedia() : Int {
    {
      let soma : Int <- 0;
      let quantidade : Int <- self.notas.length;
      let media : Int <- 0;
      
      if quantidade = 0 then
        0
      else
        {
          let i : Int <- 0;
          soma <- self.notas[0];
          
          while i < quantidade loop
            {
              soma <- soma + self.notas[i];
              i <- i + 1;
            }
          pool;
          
          media <- soma / quantidade;
          media;
        }
    }
  }
  
  metodo imprimirInformacoes() : Object {
    {
      out_string("Nome: ");
      out_string(self.nome);
      out_string("\n");
      
      out_string("Idade: ");
      out_int(self.idade);
      out_string("\n");
      
      out_string("Notas: ");
      self.notas.iterate(
        {
          (nota : Int) => {
            out_int(nota);
            out_string(" ");
          }
        }
      );
      
      out_string("\n");
      
      out_string("Média: ");
      out_int(self.calcularMedia());
      out_string("\n");
    }
  }
};

class Escola {
  atributo alunos : List<Aluno>;
  
  metodo init() : SELF_TYPE {
    {
      self.alunos <- new List<Aluno>;
      self;
    }
  }
  
  metodo adicionarAluno(aluno : Aluno) : SELF_TYPE {
    {
      self.alunos.append(aluno);
      self;
    }
  }
  
  metodo imprimirInformacoes() : Object {
    {
      self.alunos.iterate(
        {
          (aluno : Aluno) => {
            aluno.imprimirInformacoes();
          }
        }
      );
    }
  }
};

metodo main() : Object {
  {
    let escola : Escola <- new Escola;
    
    let aluno1 : Aluno <- new Aluno.init("João", 15).adicionarNota(8).adicionarNota(7).adicionarNota(9);
    escola.adicionarAluno(aluno1);
    
    let aluno2 : Aluno <- new Aluno.init("Maria", 16).adicionarNota(9).adicionarNota(9).adicionarNota(10);
    escola.adicionarAluno(aluno2);
    
    escola.imprimirInformacoes();
  }
};
```

Este código em COOL implementa duas classes: `Aluno` e `Escola`. A classe `Aluno` possui três atributos: `nome` (do tipo `String`), `idade` (do tipo `Int`) e `notas` (do tipo `List<Int>`). Ela também possui métodos para inicialização, adicionar notas, calcular a média e imprimir as informações do aluno.

A classe `Escola` possui um atributo `alunos` (do tipo `List<Aluno>`) e métodos para inicialização, adicionar alunos e imprimir as informações de todos os alunos.

No método `main`, são criados dois objetos `Aluno` e adicionados à instância de `Escola`. Em seguida, as informações de todos os alunos são impressas.

Este código mostra um exemplo básico de como usar a orientação a objetos e as estruturas de controle em COOL para criar um sistema de gerenciamento de alunos em uma escola.