Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de biblioteca:

```
class Livro {
    atributo titulo : String <- ""
    atributo autor : String <- ""
    atributo anoPublicacao : Int <- 0
    atributo quantidade : Int <- 0

    metodo init(t : String, a : String, ano : Int, qtd : Int) : SELF_TYPE {
        {
            titulo <- t;
            autor <- a;
            anoPublicacao <- ano;
            quantidade <- qtd;
            retorno self;
        }
    }

    metodo emprestar() : String {
        se quantidade > 0 entao
            quantidade <- quantidade - 1;
            retorno "Livro emprestado com sucesso!";
        senao
            retorno "Não temos mais este livro disponível para empréstimo.";
        fim;
    }

    metodo devolver() : String {
        quantidade <- quantidade + 1;
        retorno "Livro devolvido com sucesso!";
    }

    metodo exibirInformacoes() : String {
        retorno "Título: " + titulo + "\n" +
                "Autor: " + autor + "\n" +
                "Ano de Publicação: " + anoPublicacao + "\n" +
                "Quantidade Disponível: " + quantidade;
    }
}

class Biblioteca {
    atributo livros : List of Livro <- nil

    metodo adicionarLivro(l : Livro) : SELF_TYPE {
        se livros = nil entao
            livros <- l @ [ ]
        senao
            livros <- livros @ l @ [ ];
        fim;
        retorno self;
    }

    metodo buscarLivro(titulo : String) : String {
        para cada livro em livros faca
            se livro.titulo = titulo entao
                retorno livro.exibirInformacoes();
            fim;
        fim;

        retorno "Livro não encontrado.";
    }

    metodo exibirLivros() : String {
        se livros = nil entao
            retorno "Não há livros cadastrados.";
        senao
            para cada livro em livros faca
                exibir livro.exibirInformacoes();
                exibir "---------------------------";
            fim;
        fim;
    }
}

class Main {
    metodo main() : Object {
        var biblioteca : Biblioteca <- Biblioteca.new;

        var livro1 : Livro <- Livro.new.init("Dom Quixote", "Miguel de Cervantes", 1605, 2);
        var livro2 : Livro <- Livro.new.init("1984", "George Orwell", 1949, 4);
        var livro3 : Livro <- Livro.new.init("Cem Anos de Solidão", "Gabriel García Márquez", 1967, 1);

        biblioteca.adicionarLivro(livro1).adicionarLivro(livro2).adicionarLivro(livro3);

        exibir biblioteca.buscarLivro("1984");
        exibir biblioteca.buscarLivro("Cem Anos de Solidão");
        exibir biblioteca.buscarLivro("Dom Quixote");

        exibir "---------------------------";

        exibir biblioteca.exibirLivros();

        retorno void;
    }
}
```

Neste código, temos três classes principais: `Livro`, `Biblioteca` e `Main`. A classe `Livro` possui atributos como título, autor, ano de publicação e quantidade disponível. Ela também possui métodos para emprestar, devolver e exibir informações sobre o livro.

A classe `Biblioteca` possui uma lista de livros e métodos para adicionar livros, buscar um livro pelo título e exibir todos os livros cadastrados.

A classe `Main` é a classe principal que instancia a biblioteca, cria alguns livros e realiza algumas operações, como buscar livros e exibir informações.

É importante ressaltar que o COOL é uma linguagem fictícia usada apenas para fins educacionais.