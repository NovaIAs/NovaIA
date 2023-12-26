Claro! Vou gerar um código complexo em COOL (Classroom Object-Oriented Language) para você. Este código é um exemplo de um sistema de gerenciamento de biblioteca. Ele permite que os usuários adicionem livros à biblioteca, consultem livros disponíveis, emprestem livros, devolvam livros e gerenciem multas por atraso.

Veja abaixo o código completo:

```
class Biblioteca {
    atributo
        livrosDisponiveis : Lista(Livro)
        livrosEmprestados : Lista(Livro)
        multas : Lista(Multa)

    metodo adicionarLivro(livro : Livro) : Nada {
        se nao self.livrosDisponiveis.contem(livro) entao
            self.livrosDisponiveis.adicionar(livro)
        senao
            imprimir("Livro já está na biblioteca.")
        fim
    }

    metodo consultarLivrosDisponiveis() : Nada {
        se self.livrosDisponiveis.vazia() entao
            imprimir("Não há livros disponíveis.")
        senao
            imprimir("Livros disponíveis:")
            para cada livro em self.livrosDisponiveis faça
                imprimir(livro.titulo)
            fim
        fim
    }

    metodo emprestarLivro(livro : Livro, usuario : Usuario) : Nada {
        se self.livrosDisponiveis.contem(livro) entao
            self.livrosDisponiveis.remover(livro)
            self.livrosEmprestados.adicionar(livro)
            imprimir("Livro emprestado com sucesso.")
        senao
            imprimir("Livro indisponível para empréstimo.")
        fim
    }

    metodo devolverLivro(livro : Livro) : Nada {
        se self.livrosEmprestados.contem(livro) entao
            self.livrosEmprestados.remover(livro)
            self.livrosDisponiveis.adicionar(livro)
            imprimir("Livro devolvido com sucesso.")
        senao
            imprimir("Livro não está emprestado.")
        fim
    }

    metodo adicionarMulta(usuario : Usuario, valor : Real) : Nada {
        multa : Multa <- Multa(usuario, valor)
        self.multas.adicionar(multa)
        imprimir("Multa adicionada com sucesso.")
    }

    metodo consultarMultas(usuario : Usuario) : Nada {
        multasUsuario : Lista(Multa) <- self.multas.filtrar(lambda m | m.usuario = usuario)
        se multasUsuario.vazia() entao
            imprimir("Usuário não possui multas.")
        senao
            imprimir("Multas do usuário:")
            para cada multa em multasUsuario faça
                imprimir("Valor: " + multa.valor)
            fim
        fim
    }
}

class Livro {
    atributo
        titulo : String
        autor : String

    construtor novo(titulo : String, autor : String) {
        self.titulo <- titulo
        self.autor <- autor
    }
}

class Usuario {
    atributo
        nome : String

    construtor novo(nome : String) {
        self.nome <- nome
    }
}

class Multa {
    atributo
        usuario : Usuario
        valor : Real

    construtor novo(usuario : Usuario, valor : Real) {
        self.usuario <- usuario
        self.valor <- valor
    }
}

programa {
    biblioteca : Biblioteca <- Biblioteca()

    livro1 : Livro <- Livro.novo("Harry Potter e a Pedra Filosofal", "J.K. Rowling")
    livro2 : Livro <- Livro.novo("1984", "George Orwell")
    livro3 : Livro <- Livro.novo("O Senhor dos Anéis", "J.R.R. Tolkien")

    usuario1 : Usuario <- Usuario.novo("João")
    usuario2 : Usuario <- Usuario.novo("Maria")

    biblioteca.adicionarLivro(livro1)
    biblioteca.adicionarLivro(livro2)
    biblioteca.adicionarLivro(livro3)

    biblioteca.consultarLivrosDisponiveis()

    biblioteca.emprestarLivro(livro1, usuario1)
    biblioteca.emprestarLivro(livro2, usuario2)

    biblioteca.consultarLivrosDisponiveis()

    biblioteca.devolverLivro(livro1)
    biblioteca.devolverLivro(livro2)

    biblioteca.consultarLivrosDisponiveis()

    biblioteca.adicionarMulta(usuario1, 10.0)
    biblioteca.adicionarMulta(usuario2, 15.0)

    biblioteca.consultarMultas(usuario1)
    biblioteca.consultarMultas(usuario2)
}
```

Este código em COOL implementa um sistema básico de gerenciamento de biblioteca. Ele inclui três classes principais: Biblioteca, Livro e Usuario. A classe Biblioteca é responsável por adicionar livros, consultar livros disponíveis, emprestar livros, devolver livros, adicionar multas e consultar multas. A classe Livro possui atributos para o título e autor do livro. A classe Usuario possui um atributo para o nome do usuário. A classe Multa representa as multas associadas a um usuário e possui atributos para o usuário e o valor da multa.

O programa principal cria uma instância da classe Biblioteca e algumas instâncias de livros e usuários. Em seguida, ele chama os métodos da biblioteca para adicionar livros, consultar livros disponíveis, emprestar livros, devolver livros, adicionar multas e consultar multas.

Espero que este exemplo complexo em COOL atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.