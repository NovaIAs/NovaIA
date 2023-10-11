Claro! Aqui está um exemplo de código em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de biblioteca:

```
class Livro {
    atributo titulo : String;
    atributo autor : String;
    atributo anoPublicacao : Int;
    atributo disponivel : Bool;

    metodo init(titulo : String, autor : String, anoPublicacao : Int) : SELF_TYPE {
        self.titulo <- titulo;
        self.autor <- autor;
        self.anoPublicacao <- anoPublicacao;
        self.disponivel <- true;
        return self;
    }

    metodo emprestar() : Bool {
        se self.disponivel entao
            self.disponivel <- false;
            retorna true;
        senao
            retorna false;
        fim;
    }

    metodo devolver() : Bool {
        se nao self.disponivel entao
            self.disponivel <- true;
            retorna true;
        senao
            retorna false;
        fim;
    }
}

class Biblioteca {
    atributo livros : List of Livro;

    metodo init() : SELF_TYPE {
        self.livros <- nil;
        return self;
    }

    metodo adicionarLivro(livro : Livro) : Bool {
        se self.livros.contem(livro) entao
            retorna false;
        senao
            self.livros <- self.livros.adicionar(livro);
            retorna true;
        fim;
    }

    metodo removerLivro(livro : Livro) : Bool {
        se self.livros.contem(livro) entao
            self.livros <- self.livros.remover(livro);
            retorna true;
        senao
            retorna false;
        fim;
    }

    metodo listarLivrosDisponiveis() : List of Livro {
        var livrosDisponiveis : List of Livro <- new List of Livro;
        para cada livro em self.livros faca
            se livro.disponivel entao
                livrosDisponiveis.adicionar(livro);
            fim;
        fim;
        retorna livrosDisponiveis;
    }
}
```

Neste código em COOL, temos duas classes principais: `Livro` e `Biblioteca`. 

A classe `Livro` representa um livro na biblioteca e possui atributos como `titulo`, `autor`, `anoPublicacao` e `disponivel`. O método `init` é usado para inicializar os atributos do livro. Os métodos `emprestar` e `devolver` controlam o status de disponibilidade do livro.

A classe `Biblioteca` representa a própria biblioteca e possui um atributo `livros` que é uma lista de livros. O método `init` é usado para inicializar a lista de livros. Os métodos `adicionarLivro` e `removerLivro` adicionam e removem livros da lista, respectivamente. Já o método `listarLivrosDisponiveis` retorna uma lista com todos os livros disponíveis na biblioteca.

Esse código é apenas um exemplo básico de implementação de um sistema de gerenciamento de biblioteca em COOL. É possível adicionar mais funcionalidades e métodos de acordo com as necessidades específicas do projeto.