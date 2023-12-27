Claro! Aqui está um código em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de biblioteca. Este código é bastante complexo e possui várias classes e funcionalidades diferentes. 

```cool
class Livro {
  atributo titulo : String;
  atributo autor : String;
  atributo ano : Int;
  atributo emprestado : Bool;
  
  metodo inicializa(t : String, a : String, an : Int) : Livro {
    titulo <- t;
    autor <- a;
    ano <- an;
    emprestado <- false;
    retorne self;
  };
  
  metodo empresta() : Livro {
    se emprestado então
      erro("O livro já está emprestado.");
    fimse;
    
    emprestado <- true;
    retorne self;
  };
  
  metodo devolve() : Livro {
    se não emprestado então
      erro("O livro não está emprestado.");
    fimse;
    
    emprestado <- false;
    retorne self;
  };
  
  metodo toString() : String {
    var resultado : String;
    resultado <- "Título: " + titulo + "\n";
    resultado <- resultado + "Autor: " + autor + "\n";
    resultado <- resultado + "Ano: " + ano.toString() + "\n";
    se emprestado então
      resultado <- resultado + "Status: emprestado\n";
    senão
      resultado <- resultado + "Status: disponível\n";
    fimse;
    retorne resultado;
  };
};

class Biblioteca {
  atributo livros : List of Livro;
  
  metodo inicializa() : Biblioteca {
    livros <- new List of Livro;
    retorne self;
  };
  
  metodo adicionaLivro(l : Livro) : Biblioteca {
    livros.append(l);
    retorne self;
  };
  
  metodo removeLivro(l : Livro) : Biblioteca {
    livros.remove(l);
    retorne self;
  };
  
  metodo buscaLivro(titulo : String) : Livro {
    para cada l em livros faça
      se l.titulo = titulo então
        retorne l;
      fimse;
    fimpara;
    retorne null;
  };
  
  metodo listaLivros() : String {
    var resultado : String;
    resultado <- "Lista de Livros:\n";
    para cada l em livros faça
      resultado <- resultado + l.toString();
    fimpara;
    retorne resultado;
  };
};

metodo principal() : Object {
  var biblioteca : Biblioteca;
  biblioteca <- new Biblioteca.inicializa();
  
  var livro1 : Livro;
  livro1 <- new Livro.inicializa("A Revolução dos Bichos", "George Orwell", 1945);
  biblioteca.adicionaLivro(livro1);
  
  var livro2 : Livro;
  livro2 <- new Livro.inicializa("1984", "George Orwell", 1949);
  biblioteca.adicionaLivro(livro2);
  
  var livro3 : Livro;
  livro3 <- new Livro.inicializa("O Senhor dos Anéis: A Sociedade do Anel", "J.R.R. Tolkien", 1954);
  biblioteca.adicionaLivro(livro3);
  
  var livro4 : Livro;
  livro4 <- new Livro.inicializa("O Hobbit", "J.R.R. Tolkien", 1937);
  biblioteca.adicionaLivro(livro4);
  
  escreva(biblioteca.listaLivros());
  
  var livroEmprestado : Livro;
  livroEmprestado <- biblioteca.buscaLivro("A Revolução dos Bichos");
  livroEmprestado.empresta();
  
  var livroDevolvido : Livro;
  livroDevolvido <- biblioteca.buscaLivro("O Hobbit");
  livroDevolvido.devolve();
  
  escreva(biblioteca.listaLivros());
  
  retorne 0;
};
```

Este código em COOL implementa um sistema de gerenciamento de biblioteca, com as classes `Livro` e `Biblioteca`. A classe `Livro` possui atributos como `titulo`, `autor`, `ano` e `emprestado`. Além disso, possui métodos para inicialização, empréstimo, devolução e representação em formato de string.

A classe `Biblioteca` possui um atributo `livros` que é uma lista de objetos da classe `Livro`. Ela também possui métodos para inicialização, adição e remoção de livros, busca por título e listagem de todos os livros.

No método `principal`, é criada uma instância da classe `Biblioteca` e alguns livros são adicionados a ela. Em seguida, é feita a impressão da lista de livros. Depois, um livro é emprestado e outro é devolvido, e a lista de livros é novamente impressa.

Espero que este código complexo em COOL atenda à sua solicitação!