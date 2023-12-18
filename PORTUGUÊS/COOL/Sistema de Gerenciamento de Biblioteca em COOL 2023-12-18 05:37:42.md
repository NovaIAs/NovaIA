Claro, vou criar um código em COOL (Classroom Object-Oriented Language) que simula um sistema de gerenciamento de biblioteca. Neste sistema, será possível cadastrar livros, pesquisar livros por título e autor, emprestar e devolver livros, bem como exibir informações sobre os livros disponíveis.

```
class Livro {

    string titulo;
    string autor;
    bool disponivel;

    new(t: AUTO_TYPE, a: AUTO_TYPE) : Livro {
        titulo <- t;
        autor <- a;
        disponivel <- true;
        self;
    };

    metodo getTitulo() : AUTO_TYPE {
        titulo;
    };

    metodo getAutor() : AUTO_TYPE {
        autor;
    };

    metodo estaDisponivel() : AUTO_TYPE {
        disponivel;
    };

    metodo emprestar() : AUTO_TYPE {
        se disponivel entao
            disponivel <- false;
            "Livro emprestado com sucesso!";
        senao
            "Livro indisponível para empréstimo.";
        fim;
    };

    metodo devolver() : AUTO_TYPE {
        se nao disponivel entao
            disponivel <- true;
            "Livro devolvido com sucesso!";
        senao
            "O livro já está disponível.";
        fim;
    };
};

class Biblioteca {

    listaLivros: List(Livro);

    new() : Biblioteca {
        listaLivros <- new List(Livro);
        self;
    };

    metodo cadastrarLivro(titulo: AUTO_TYPE, autor: AUTO_TYPE) : AUTO_TYPE {
        livro: Livro <- new Livro(titulo, autor);
        listaLivros.insertLast(livro);
        "Livro cadastrado com sucesso!";
    };

    metodo pesquisarLivroPorTitulo(titulo: AUTO_TYPE) : AUTO_TYPE {
        para livro em listaLivros faca
            se livro.getTitulo() = titulo entao
                retornar livro;
            fim;
        fim;
        retornar null;
    };

    metodo pesquisarLivroPorAutor(autor: AUTO_TYPE) : AUTO_TYPE {
        livrosEncontrados: List(Livro) <- new List(Livro);
        para livro em listaLivros faca
            se livro.getAutor() = autor entao
                livrosEncontrados.insertLast(livro);
            fim;
        fim;
        retornar livrosEncontrados;
    };

    metodo emprestarLivro(titulo: AUTO_TYPE) : AUTO_TYPE {
        livro: Livro <- pesquisarLivroPorTitulo(titulo);
        se livro != null entao
            livro.emprestar();
        senao
            "Livro não encontrado.";
        fim;
    };

    metodo devolverLivro(titulo: AUTO_TYPE) : AUTO_TYPE {
        livro: Livro <- pesquisarLivroPorTitulo(titulo);
        se livro != null entao
            livro.devolver();
        senao
            "Livro não encontrado.";
        fim;
    };

    metodo exibirLivrosDisponiveis() : AUTO_TYPE {
        livrosDisponiveis: List(Livro) <- new List(Livro);
        para livro em listaLivros faca
            se livro.estaDisponivel() entao
                livrosDisponiveis.insertLast(livro);
            fim;
        fim;
        retornar livrosDisponiveis;
    };
};

// Exemplo de uso:

biblioteca: Biblioteca <- new Biblioteca();

biblioteca.cadastrarLivro("Dom Casmurro", "Machado de Assis");
biblioteca.cadastrarLivro("O Pequeno Príncipe", "Antoine de Saint-Exupéry");

biblioteca.emprestarLivro("Dom Casmurro");
biblioteca.devolverLivro("O Pequeno Príncipe");

livroEncontrado: Livro <- biblioteca.pesquisarLivroPorTitulo("Dom Casmurro");
se livroEncontrado != null entao
    escreva("Livro encontrado: " + livroEncontrado.getTitulo() + " - " + livroEncontrado.getAutor());
senao
    escreva("Livro não encontrado.");
fim;

livrosDisponiveis: List(Livro) <- biblioteca.exibirLivrosDisponiveis();
para livro em livrosDisponiveis faca
    escreva("Livro disponível: " + livro.getTitulo() + " - " + livro.getAutor());
fim;
```

Neste código, temos duas classes: `Livro` e `Biblioteca`. A classe `Livro` possui atributos como título, autor e disponibilidade, bem como métodos para obter essas informações, emprestar e devolver um livro. A classe `Biblioteca` é responsável por gerenciar a lista de livros, permitindo cadastrar livros, pesquisar por título e autor, emprestar e devolver livros, e exibir os livros disponíveis.

No exemplo de uso no final do código, é criada uma instância da classe `Biblioteca`, são cadastrados dois livros, é realizado um empréstimo e uma devolução, e são exibidos os livros disponíveis.