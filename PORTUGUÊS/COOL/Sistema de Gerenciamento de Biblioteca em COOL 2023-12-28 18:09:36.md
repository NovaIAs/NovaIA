Claro! Aqui está um exemplo de código em COOL que envolve um sistema de gerenciamento de biblioteca. 

```
class Biblioteca {

    atributo livros : Lista(Livro)
    atributo usuarios : Lista(Usuario)

    metodo adicionarLivro(livro : Livro) : Livro {
        livros.adicionar(livro)
        retornar livro
    }

    metodo removerLivro(livro : Livro) : Livro {
        livros.remover(livro)
        retornar livro
    }

    metodo emprestarLivro(livro : Livro, usuario : Usuario) : Livro {
        se livro.disponivel() entao
            livro.emprestar(usuario)
            retornar livro
        senao
            imprimir("Este livro não está disponível para empréstimo.")
            retornar nulo
        fim
    }

    metodo devolverLivro(livro : Livro) : Livro {
        livro.devolver()
        retornar livro
    }

    metodo cadastrarUsuario(usuario : Usuario) : Usuario {
        usuarios.adicionar(usuario)
        retornar usuario
    }

    metodo removerUsuario(usuario : Usuario) : Usuario {
        usuarios.remover(usuario)
        retornar usuario
    }

    metodo buscarLivroPorTitulo(titulo : String) : Lista(Livro) {
        listaEncontrados : Lista(Livro)
        para cada livro em livros faça
            se livro.titulo == titulo entao
                listaEncontrados.adicionar(livro)
            fim
        fim
        retornar listaEncontrados
    }

    metodo buscarLivroPorAutor(autor : String) : Lista(Livro) {
        listaEncontrados : Lista(Livro)
        para cada livro em livros faça
            se livro.autor == autor entao
                listaEncontrados.adicionar(livro)
            fim
        fim
        retornar listaEncontrados
    }

}

classe Livro {

    atributo titulo : String
    atributo autor : String
    atributo disponivel : Booleano

    metodo emprestar(usuario : Usuario) {
        disponivel <- falso
        usuario.adicionarLivroEmprestado(this)
    }

    metodo devolver() {
        disponivel <- verdadeiro
    }

    metodo disponivel() : Booleano {
        retornar disponivel
    }

}

classe Usuario {

    atributo nome : String
    atributo livrosEmprestados : Lista(Livro)

    metodo adicionarLivroEmprestado(livro : Livro) {
        livrosEmprestados.adicionar(livro)
    }

    metodo removerLivroEmprestado(livro : Livro) {
        livrosEmprestados.remover(livro)
    }

}
```

Neste código, temos três classes principais: Biblioteca, Livro e Usuário. A classe Biblioteca é responsável por gerenciar a lista de livros e usuários, além de fornecer métodos para adicionar, remover e buscar livros ou usuários. A classe Livro representa um livro na biblioteca, com atributos como título, autor e disponibilidade. A classe Usuário representa um usuário da biblioteca, com atributos como nome e lista de livros emprestados.

Através dos métodos da classe Biblioteca, é possível adicionar, remover, emprestar e devolver livros, assim como cadastrar e remover usuários. Também é possível buscar livros por título ou autor.

Espero que este exemplo de código em COOL seja útil para você!