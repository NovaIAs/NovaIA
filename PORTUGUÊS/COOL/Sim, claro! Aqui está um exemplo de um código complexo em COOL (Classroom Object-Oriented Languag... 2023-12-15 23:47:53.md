Claro! Aqui está um exemplo de um código complexo em COOL (Classroom Object-Oriented Language) que simula um sistema de gerenciamento de biblioteca:

```
class Livro {
    atributo titulo : String;
    atributo autor : String;
    atributo anoPublicacao : Int;
    atributo disponivel : Bool;
    atributo emprestadoPara : String;

    metodo inicializar(t : String, a : String, ano : Int) : SELF_TYPE {
        {
            self.titulo <- t;
            self.autor <- a;
            self.anoPublicacao <- ano;
            self.disponivel <- TRUE;
            self.emprestadoPara <- "";
            self;
        }
    }

    metodo emprestar(nome : String) : String {
        if self.disponivel then
            {
                self.disponivel <- FALSE;
                self.emprestadoPara <- nome;
                "Livro '" + self.titulo + "' emprestado para " + nome + ".";
            }
        else
            {
                "O livro '" + self.titulo + "' não está disponível no momento.";
            }
    }

    metodo devolver() : String {
        if self.disponivel then
            {
                "O livro '" + self.titulo + "' já está disponível.";
            }
        else
            {
                self.disponivel <- TRUE;
                "Livro '" + self.titulo + "' devolvido.";
            }
    }
}

class Biblioteca {
    atributo livros : List of Livro;

    metodo adicionarLivro(l : Livro) : SELF_TYPE {
        {
            self.livros <- self.livros + l;
            self;
        }
    }

    metodo listarLivros() : String {
        let resultado : String <- "";

        for i : Int <- 0 while i < length(self.livros) loop
            {
                resultado <- resultado + "Titulo: " + self.livros[i].titulo + "\n";
                resultado <- resultado + "Autor: " + self.livros[i].autor + "\n";
                resultado <- resultado + "Ano de Publicacao: " + self.livros[i].anoPublicacao + "\n";
                resultado <- resultado + "Disponivel: " + (if self.livros[i].disponivel then "Sim" else "Não") + "\n";
                resultado <- resultado + "Emprestado para: " + (if self.livros[i].emprestadoPara = "" then "Ninguém" else self.livros[i].emprestadoPara) + "\n";
                resultado <- resultado + "-----------------------\n";
            }
        pool;

        resultado;
    }
}

class Main {
    atributo biblioteca : Biblioteca;

    metodo main() : Object {
        {
            let livro1 : Livro <- Livro.inicializar("Dom Quixote", "Miguel de Cervantes", 1605);
            let livro2 : Livro <- Livro.inicializar("Orgulho e Preconceito", "Jane Austen", 1813);
            let livro3 : Livro <- Livro.inicializar("1984", "George Orwell", 1949);

            self.biblioteca <- Biblioteca.new.adicionarLivro(livro1).adicionarLivro(livro2).adicionarLivro(livro3);

            outputString(self.biblioteca.listarLivros());
            outputString(livro1.emprestar("João"));
            outputString(self.biblioteca.listarLivros());
            outputString(livro2.emprestar("Maria"));
            outputString(self.biblioteca.listarLivros());
            outputString(livro1.devolver());
            outputString(self.biblioteca.listarLivros());
        }
    }
};
```

Neste código em COOL, temos três classes: `Livro`, `Biblioteca` e `Main`. A classe `Livro` representa um livro com atributos como título, autor, ano de publicação, disponibilidade e quem o emprestou. Ela possui métodos para inicializar um livro, emprestá-lo e devolvê-lo.

A classe `Biblioteca` representa uma coleção de livros e possui um atributo `livros` que é uma lista de objetos da classe `Livro`. Ela possui métodos para adicionar livros à biblioteca e listar os livros disponíveis.

A classe `Main` é a classe principal que contém o método `main()`, onde criamos três objetos da classe `Livro`, adicionamos eles à biblioteca e realizamos algumas operações como empréstimo e devolução de livros.

Espero que este exemplo atenda às suas expectativas!