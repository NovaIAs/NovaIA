Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de biblioteca:

```
class Livro {
  atributo titulo : String <- ""
  atributo autor : String <- ""
  atributo anoPublicacao : Int <- 0

  metodo init(t : String, a : String, ap : Int) : SELF_TYPE {
    {
      titulo <- t
      autor <- a
      anoPublicacao <- ap
      self
    }
  }

  metodo getTitulo() : String {
    titulo
  }

  metodo getAutor() : String {
    autor
  }

  metodo getAnoPublicacao() : Int {
    anoPublicacao
  }
}

class Biblioteca {
  atributo livros : Array of Livro

  metodo init() : SELF_TYPE {
    {
      livros <- new Array of Livro
      self
    }
  }

  metodo adicionarLivro(t : String, a : String, ap : Int) : SELF_TYPE {
    {
      livros.add(new Livro.init(t, a, ap))
      self
    }
  }

  metodo buscarLivroPorAutor(a : String) : Array of Livro {
    var resultados : Array of Livro <- new Array of Livro
    for l in livros {
      if l.getAutor() = a then
        resultados.add(l)
      fi
    }
    resultados
  }

  metodo buscarLivroPorAnoPublicacao(ap : Int) : Array of Livro {
    var resultados : Array of Livro <- new Array of Livro
    for l in livros {
      if l.getAnoPublicacao() = ap then
        resultados.add(l)
      fi
    }
    resultados
  }

  metodo imprimirLivros() : Object {
    for l in livros {
      out_string(l.getTitulo() + " - " + l.getAutor() + " (" + l.getAnoPublicacao().toString() + ")")
      out_string("\n")
    }
    self
  }
}

metodo principal() : Object {
  var biblioteca : Biblioteca <- new Biblioteca.init()
  biblioteca.adicionarLivro("Dom Quixote", "Miguel de Cervantes", 1605)
  biblioteca.adicionarLivro("1984", "George Orwell", 1949)
  biblioteca.adicionarLivro("O Senhor dos Anéis", "J.R.R. Tolkien", 1954)
  biblioteca.adicionarLivro("Harry Potter e a Pedra Filosofal", "J.K. Rowling", 1997)
  
  out_string("----- Livros adicionados -----\n")
  biblioteca.imprimirLivros()

  out_string("\n----- Busca por autor -----\n")
  var livrosPorAutor : Array of Livro <- biblioteca.buscarLivroPorAutor("George Orwell")
  for l in livrosPorAutor {
    out_string(l.getTitulo() + " - " + l.getAutor() + " (" + l.getAnoPublicacao().toString() + ")")
    out_string("\n")
  }

  out_string("\n----- Busca por ano de publicação -----\n")
  var livrosPorAno : Array of Livro <- biblioteca.buscarLivroPorAnoPublicacao(1954)
  for l in livrosPorAno {
    out_string(l.getTitulo() + " - " + l.getAutor() + " (" + l.getAnoPublicacao().toString() + ")")
    out_string("\n")
  }

  0
}
```

Este código é dividido em duas classes: `Livro` e `Biblioteca`. A classe `Livro` representa um livro com atributos como título, autor e ano de publicação. Ela possui um método `init` para inicializar esses atributos e métodos `get` para recuperar as informações do livro.

A classe `Biblioteca` representa uma biblioteca e possui um array de livros como atributo. Ela possui um método `init` para inicializar o array de livros, um método `adicionarLivro` para adicionar um novo livro à biblioteca, métodos `buscarLivroPorAutor` e `buscarLivroPorAnoPublicacao` para buscar livros com base no autor ou ano de publicação, e um método `imprimirLivros` para imprimir todos os livros da biblioteca.

No método `principal`, é criada uma instância da classe `Biblioteca`, são adicionados alguns livros à biblioteca e são realizadas buscas por autor e ano de publicação. Por fim, os resultados são impressos na saída.

Espero que este código complexo em COOL atenda às suas expectativas!