Claro! Aqui está um código em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de biblioteca. Este código é complexo e diferenciado, e foi escrito em português para facilitar a compreensão:

```
class Livro {

  atributo
    titulo : String,
    autor : String,
    ano : Int,
    disponivel : Bool,
    emprestadoPor : String

  metodos
    inicializar(t : String, a : String, an : Int) : AUTO_TYPE {
      {
        titulo <- t;
        autor <- a;
        ano <- an;
        disponivel <- true;
        emprestadoPor <- ""
      }
    };

    emprestar(nome : String) : AUTO_TYPE {
      {
        if disponivel then
          {
            disponivel <- false;
            emprestadoPor <- nome;
            "Livro emprestado com sucesso!" @ coolout
          }
        else
          "Livro indisponível para empréstimo" @ coolout
      }
    };

    devolver() : AUTO_TYPE {
      {
        if disponivel then
          "Este livro já está disponível na biblioteca" @ coolout
        else
          {
            disponivel <- true;
            emprestadoPor <- "";
            "Livro devolvido com sucesso!" @ coolout
          }
      }
    };

    imprimirDetalhes() : AUTO_TYPE {
      {
        "Título: " @ coolout;
        titulo @ coolout;
        "Autor: " @ coolout;
        autor @ coolout;
        "Ano: " @ coolout;
        ano.toString @ coolout;
        "Disponível: " @ coolout;
        disponivel.toString @ coolout;
        if not disponivel then
          {
            "Emprestado por: " @ coolout;
            emprestadoPor @ coolout
          }
      }
    }
};

class Biblioteca {

  atributo livros : AUTO_TYPE

  metodos
    inicializar() : AUTO_TYPE {
      {
        livros <- new Map[String, Livro];
        "Biblioteca inicializada com sucesso!" @ coolout
      }
    };

    adicionarLivro(t : String, a : String, an : Int) : AUTO_TYPE {
      {
        let novoLivro : Livro <- new Livro;
        novoLivro.inicializar(t, a, an);
        livros[t] <- novoLivro;
        "Livro adicionado à biblioteca!" @ coolout
      }
    };

    removerLivro(t : String) : AUTO_TYPE {
      {
        if livros.has_key(t) then
          {
            livros.remove(t);
            "Livro removido da biblioteca!" @ coolout
          }
        else
          "Livro não encontrado na biblioteca" @ coolout
      }
    };

    emprestarLivro(t : String, nome : String) : AUTO_TYPE {
      {
        if livros.has_key(t) then
          livros[t].emprestar(nome)
        else
          "Livro não encontrado na biblioteca" @ coolout
      }
    };

    devolverLivro(t : String) : AUTO_TYPE {
      {
        if livros.has_key(t) then
          livros[t].devolver()
        else
          "Livro não encontrado na biblioteca" @ coolout
      }
    };

    imprimirLivros() : AUTO_TYPE {
      {
        for livro in livros.values do
          {
            livro.imprimirDetalhes();
            "\n" @ coolout
          }
      }
    }
};

class Main {

  metodos
    main() : AUTO_TYPE {
      {
        let biblioteca : Biblioteca <- new Biblioteca;
        biblioteca.inicializar();

        biblioteca.adicionarLivro("Dom Quixote", "Miguel de Cervantes", 1605);
        biblioteca.adicionarLivro("Cem Anos de Solidão", "Gabriel García Márquez", 1967);
        biblioteca.adicionarLivro("1984", "George Orwell", 1949);

        biblioteca.imprimirLivros();

        biblioteca.emprestarLivro("1984", "João");
        biblioteca.devolverLivro("Cem Anos de Solidão");

        biblioteca.imprimirLivros()
      }
    }
};
```

Nesse código, criamos três classes: `Livro`, `Biblioteca` e `Main`. 

A classe `Livro` representa um livro na biblioteca, com atributos como título, autor, ano de publicação, disponibilidade e quem o emprestou. Os métodos `inicializar`, `emprestar`, `devolver` e `imprimirDetalhes` são responsáveis por realizar as operações de empréstimo, devolução e exibição de informações do livro.

A classe `Biblioteca` representa a biblioteca em si, com um atributo `livros` que é um mapa de livros. Os métodos `inicializar`, `adicionarLivro`, `removerLivro`, `emprestarLivro`, `devolverLivro` e `imprimirLivros` permitem inicializar a biblioteca, adicionar e remover livros, emprestar e devolver livros, e imprimir a lista de livros com seus detalhes.

A classe `Main` é a responsável por iniciar o programa. No método `main`, criamos uma instância da biblioteca, inicializamos a biblioteca, adicionamos alguns livros, imprimimos a lista de livros, emprestamos um livro e devolvemos outro, e por fim, imprimimos a lista de livros novamente.

Esse código demonstra um sistema simples de gerenciamento de biblioteca, permitindo adicionar, remover, emprestar, devolver e imprimir informações sobre os livros.