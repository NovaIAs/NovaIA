Claro! Aqui está um código em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de biblioteca:

```
class Biblioteca {

    atributo livros : Lista(Livro)
    atributo emprestimos : Lista(Emprestimo)

    metodo adicionarLivro(livro : Livro) : Unit {
        se !livros.contem(livro) então
            livros.adicionar(livro)
        senão
            imprimir("Livro já existe na biblioteca.")
        fim
    }

    metodo removerLivro(livro : Livro) : Unit {
        se livros.contem(livro) então
            livros.remover(livro)
        senão
            imprimir("Livro não encontrado na biblioteca.")
        fim
    }

    metodo listarLivros() : Unit {
        imprimir("Lista de livros na biblioteca:")

        para cada livro em livros faça
            imprimir(livro.titulo)
        fim
    }

    metodo emprestarLivro(livro : Livro, usuario : Usuario) : Unit {
        se livros.contem(livro) então
            se livro.disponivel então
                livro.disponivel <- falso
                emprestimo <- novo Emprestimo(livro, usuario)
                emprestimos.adicionar(emprestimo)
                imprimir("Livro emprestado com sucesso.")
            senão
                imprimir("Livro já está emprestado.")
            fim
        senão
            imprimir("Livro não encontrado na biblioteca.")
        fim
    }

    metodo devolverLivro(livro : Livro) : Unit {
        se livros.contem(livro) então
            se livro.disponivel então
                imprimir("Livro já está na biblioteca.")
            senão
                livro.disponivel <- verdadeiro
                para cada emprestimo em emprestimos faça
                    se emprestimo.livro = livro então
                        emprestimos.remover(emprestimo)
                        imprimir("Livro devolvido com sucesso.")
                        parar
                    fim
                fim
            fim
        senão
            imprimir("Livro não encontrado na biblioteca.")
        fim
    }

}

class Livro {

    atributo titulo : String
    atributo autor : String
    atributo disponivel : Boolean

    construtor(titulo : String, autor : String) {
        self.titulo <- titulo
        self.autor <- autor
        self.disponivel <- verdadeiro
    }

}

class Usuario {

    atributo nome : String
    atributo email : String

    construtor(nome : String, email : String) {
        self.nome <- nome
        self.email <- email
    }

}

class Emprestimo {

    atributo livro : Livro
    atributo usuario : Usuario
    atributo dataEmprestimo : String

    construtor(livro : Livro, usuario : Usuario) {
        self.livro <- livro
        self.usuario <- usuario
        self.dataEmprestimo <- obterDataAtual()
    }

    metodo obterDataAtual() : String {
        // Implementação fictícia para obter a data atual
        retorna "DD/MM/AAAA"
    }

}
```

Explicação do código:

- A classe `Biblioteca` é responsável pelo gerenciamento dos livros e empréstimos. Ela possui atributos `livros` e `emprestimos`, que são listas de objetos do tipo `Livro` e `Emprestimo`, respectivamente.

- O método `adicionarLivro` adiciona um livro à biblioteca, desde que o livro não exista previamente na lista. Caso contrário, exibe uma mensagem de erro.

- O método `removerLivro` remove um livro da biblioteca, caso ele exista na lista. Caso contrário, exibe uma mensagem de erro.

- O método `listarLivros` exibe a lista de livros presentes na biblioteca.

- O método `emprestarLivro` permite emprestar um livro específico para um usuário. Antes de realizar o empréstimo, verifica se o livro está disponível. Caso esteja, altera seu status para indisponível, cria um objeto `Emprestimo` e adiciona-o à lista de empréstimos. Caso contrário, exibe uma mensagem informando que o livro já está emprestado.

- O método `devolverLivro` permite devolver um livro emprestado para a biblioteca. Verifica se o livro existe na biblioteca e se está emprestado. Se sim, altera seu status para disponível e remove o empréstimo correspondente da lista de empréstimos. Caso contrário, exibe uma mensagem de erro.

- A classe `Livro` representa um livro e possui atributos como `titulo`, `autor` e `disponivel`. O atributo `disponivel` indica se o livro está disponível para empréstimo ou não.

- A classe `Usuario` representa um usuário da biblioteca e possui atributos como `nome` e `email`.

- A classe `Emprestimo` representa um empréstimo realizado, contendo informações sobre o livro emprestado, o usuário e a data do empréstimo.

Esse código implementa um sistema básico de gerenciamento de biblioteca, permitindo adicionar, remover, listar, emprestar e devolver livros. É um exemplo de utilização do COOL para modelar um sistema orientado a objetos.