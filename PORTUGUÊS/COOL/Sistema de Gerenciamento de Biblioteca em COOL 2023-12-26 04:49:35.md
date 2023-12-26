Claro! Vou gerar um código em COOL que implementa um sistema de gerenciamento de biblioteca. 

```
class Livro {

    atributo título : String;
    atributo autor : String;
    atributo anoPublicacao : Inteiro;
    
    metódo init(t: String, a: String, ap: Inteiro) : AUTO_TYPE {
        título <- t;
        autor <- a;
        anoPublicacao <- ap;
        retorne isto;
    };

    metódo getTítulo() : String {
        retorne título;
    };

    metódo getAutor() : String {
        retorne autor;
    };

    metódo getAnoPublicacao() : Inteiro {
        retorne anoPublicacao;
    };

    metódo setTítulo(novoTítulo: String) {
        título <- novoTítulo;
    };

    metódo setAutor(novoAutor: String) {
        autor <- novoAutor;
    };

    metódo setAnoPublicacao(novoAno: Inteiro) {
        anoPublicacao <- novoAno;
    };
};

class Biblioteca {

    atributo livros : Array of Livro;

    metódo init() : AUTO_TYPE {
        livros <- criar Array [0] of Livro;
        retorne isto;
    };

    metódo adicionarLivro(livro: Livro) {
        livros <- livros + [livro];
    };

    metódo removerLivro(livro: Livro) {
        livros <- livros - [livro];
    };

    metódo buscarLivrosPorAutor(autor: String) : Array of Livro {
        atributo livrosEncontrados : Array of Livro;
        livrosEncontrados <- criar Array [0] of Livro;
        para cada l em livros faça
            se l.getAutor() = autor então
                livrosEncontrados <- livrosEncontrados + [l];
            fim
        fim
        retorne livrosEncontrados;
    };

    metódo buscarLivrosPorAno(ano: Inteiro) : Array of Livro {
        atributo livrosEncontrados : Array of Livro;
        livrosEncontrados <- criar Array [0] of Livro;
        para cada l em livros faça
            se l.getAnoPublicacao() = ano então
                livrosEncontrados <- livrosEncontrados + [l];
            fim
        fim
        retorne livrosEncontrados;
    };

    metódo listarLivros() {
        imprima "Lista de livros na biblioteca:";
        para cada l em livros faça
            imprima "Título: " + l.getTítulo();
            imprima "Autor: " + l.getAutor();
            imprima "Ano de Publicação: " + l.getAnoPublicacao();
            imprima "";
        fim
    };
};

metódo principal() {
    atributo biblioteca : Biblioteca;
    biblioteca <- novo Biblioteca().init();

    atributo livro1 : Livro;
    livro1 <- novo Livro().init("Harry Potter e a Pedra Filosofal", "J.K. Rowling", 1997);
    biblioteca.adicionarLivro(livro1);

    atributo livro2 : Livro;
    livro2 <- novo Livro().init("O Senhor dos Anéis: A Sociedade do Anel", "J.R.R. Tolkien", 1954);
    biblioteca.adicionarLivro(livro2);

    atributo livro3 : Livro;
    livro3 <- novo Livro().init("Cem Anos de Solidão", "Gabriel García Márquez", 1967);
    biblioteca.adicionarLivro(livro3);

    biblioteca.listarLivros();

    atributo livrosPorAutor : Array of Livro;
    livrosPorAutor <- biblioteca.buscarLivrosPorAutor("J.K. Rowling");
    imprima "Livros de J.K. Rowling encontrados:";
    para cada l em livrosPorAutor faça
        imprima "Título: " + l.getTítulo();
        imprima "Autor: " + l.getAutor();
        imprima "Ano de Publicação: " + l.getAnoPublicacao();
        imprima "";
    fim

    atributo livrosPorAno : Array of Livro;
    livrosPorAno <- biblioteca.buscarLivrosPorAno(1954);
    imprima "Livros de 1954 encontrados:";
    para cada l em livrosPorAno faça
        imprima "Título: " + l.getTítulo();
        imprima "Autor: " + l.getAutor();
        imprima "Ano de Publicação: " + l.getAnoPublicacao();
        imprima "";
    fim
};
```

Neste código, temos duas classes principais: `Livro` e `Biblioteca`. A classe `Livro` representa um livro com atributos como título, autor e ano de publicação. Ela possui métodos para retornar e alterar esses atributos. 

A classe `Biblioteca` é responsável por gerenciar uma coleção de livros. Ela possui métodos para adicionar e remover livros da biblioteca, além de buscar livros por autor ou ano de publicação. Também há um método para listar todos os livros presentes na biblioteca.

No método `principal()`, criamos uma instância da classe `Biblioteca` e adicionamos alguns livros a ela. Em seguida, chamamos o método `listarLivros()` para exibir todos os livros da biblioteca. Depois, buscamos e exibimos os livros de um autor específico e os livros de um determinado ano de publicação.

Espero que este código atenda às suas expectativas!