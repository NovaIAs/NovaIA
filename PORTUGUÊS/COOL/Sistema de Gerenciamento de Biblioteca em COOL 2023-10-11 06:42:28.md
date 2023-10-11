Certamente, vou gerar um código complexo em COOL (Classroom Object-Oriented Language), que é uma linguagem de programação orientada a objetos baseada em Classes. Este código será um sistema de gerenciamento de biblioteca.

Primeiro, vamos criar a classe `Livro` que representa um livro na biblioteca. Ela terá os atributos `titulo`, `autor` e `disponivel` para indicar se o livro está disponível para empréstimo ou não. A classe terá também os métodos `emprestar()` e `devolver()` para atualizar o status do livro.

```cool
class Livro {
    titulo : String;
    autor : String;
    disponivel : Bool;

    metodos
        emprestar() : SELF_TYPE {
            disponivel <- false;
            return self;
        };

        devolver() : SELF_TYPE {
            disponivel <- true;
            return self;
        };
};
```

Agora, vamos criar a classe `Biblioteca` que representa a biblioteca em si. Ela terá um atributo `livros` que será uma lista de objetos da classe `Livro`. A classe `Biblioteca` terá os métodos `adicionarLivro()`, `removerLivro()`, `listarLivrosDisponiveis()` e `listarLivrosEmprestados()` para gerenciar a coleção de livros.

```cool
class Biblioteca {
    livros : List[Livro];

    metodos
        adicionarLivro(titulo : String, autor : String) : SELF_TYPE {
            let novoLivro : Livro <- new Livro;
            novoLivro.titulo <- titulo;
            novoLivro.autor <- autor;
            novoLivro.disponivel <- true;
            livros <- livros.append(novoLivro);
            return self;
        };

        removerLivro(titulo : String) : SELF_TYPE {
            let indice : Int <- encontrarLivro(titulo);
            if indice >= 0 then
                livros <- livros.remove(indice);
            fi;
            return self;
        };

        encontrarLivro(titulo : String) : Int {
            let i : Int <- 0;
            let tamanho : Int <- livros.length;
            while i < tamanho loop
                if livros[i].titulo = titulo then
                    return i;
                fi;
                i <- i + 1;
            pool;
            return -1;
        };

        listarLivrosDisponiveis() : SELF_TYPE {
            let i : Int <- 0;
            let tamanho : Int <- livros.length;
            while i < tamanho loop
                if livros[i].disponivel then
                    out_string(livros[i].titulo);
                    out_string(": ");
                    out_string(livros[i].autor);
                    out_string("\n");
                fi;
                i <- i + 1;
            pool;
            return self;
        };

        listarLivrosEmprestados() : SELF_TYPE {
            let i : Int <- 0;
            let tamanho : Int <- livros.length;
            while i < tamanho loop
                if !livros[i].disponivel then
                    out_string(livros[i].titulo);
                    out_string(": ");
                    out_string(livros[i].autor);
                    out_string("\n");
                fi;
                i <- i + 1;
            pool;
            return self;
        };
};
```

Por fim, vamos criar a classe `Main` que será responsável por criar uma instância da classe `Biblioteca` e realizar algumas operações de teste.

```cool
class Main {
    metodos
        main() : Object {
            let biblioteca : Biblioteca <- new Biblioteca;
            
            biblioteca
                .adicionarLivro("Dom Casmurro", "Machado de Assis")
                .adicionarLivro("O Pequeno Príncipe", "Antoine de Saint-Exupéry")
                .adicionarLivro("1984", "George Orwell")
                .adicionarLivro("A Metamorfose", "Franz Kafka")
                .adicionarLivro("A Arte da Guerra", "Sun Tzu");

            biblioteca.listarLivrosDisponiveis();

            out_string("\n");

            biblioteca
                .emprestarLivro("Dom Casmurro")
                .emprestarLivro("1984");

            biblioteca.listarLivrosEmprestados();

            out_string("\n");

            biblioteca
                .devolverLivro("Dom Casmurro")
                .devolverLivro("1984");

            biblioteca.listarLivrosDisponiveis();

            out_string("\n");

            biblioteca.removerLivro("A Arte da Guerra");

            biblioteca.listarLivrosDisponiveis();

            return void;
        };
};
```

Este código cria uma instância da classe `Biblioteca`, adiciona alguns livros, empresta e devolve alguns livros e, por fim, remove um livro da biblioteca. Ele também exibe a lista de livros disponíveis e a lista de livros emprestados em diferentes momentos.