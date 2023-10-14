Claro! Aqui está um código complexo em Groovy que utiliza programação orientada a objetos para simular um sistema de gerenciamento de biblioteca:

```groovy
class Livro {
    String titulo
    String autor
    int anoPublicacao

    Livro(String titulo, String autor, int anoPublicacao) {
        this.titulo = titulo
        this.autor = autor
        this.anoPublicacao = anoPublicacao
    }

    String toString() {
        return "Título: $titulo, Autor: $autor, Ano de Publicação: $anoPublicacao"
    }
}

class Biblioteca {
    List<Livro> acervo

    Biblioteca() {
        this.acervo = []
    }

    void adicionarLivro(Livro livro) {
        acervo.add(livro)
        println("Livro adicionado com sucesso: $livro")
    }

    void removerLivro(Livro livro) {
        acervo.remove(livro)
        println("Livro removido com sucesso: $livro")
    }

    void listarLivros() {
        if (acervo.isEmpty()) {
            println("A biblioteca está vazia.")
        } else {
            println("Acervo da biblioteca:")
            acervo.each { livro ->
                println(livro)
            }
        }
    }
}

def biblioteca = new Biblioteca()

def livro1 = new Livro("Dom Quixote", "Miguel de Cervantes", 1605)
def livro2 = new Livro("1984", "George Orwell", 1949)
def livro3 = new Livro("O Apanhador no Campo de Centeio", "J.D. Salinger", 1951)

biblioteca.adicionarLivro(livro1)
biblioteca.adicionarLivro(livro2)
biblioteca.adicionarLivro(livro3)

biblioteca.listarLivros()

biblioteca.removerLivro(livro2)

biblioteca.listarLivros()
```

Neste código, temos duas classes principais: `Livro` e `Biblioteca`. A classe `Livro` representa um livro com os atributos `titulo`, `autor` e `anoPublicacao`. Ela possui um construtor que recebe os valores desses atributos e um método `toString()` para imprimir as informações do livro de forma formatada.

A classe `Biblioteca` representa a biblioteca, que possui um atributo `acervo`, que é uma lista de livros. Ela possui um construtor que inicializa o acervo vazio. Além disso, possui métodos como `adicionarLivro`, `removerLivro` e `listarLivros` para adicionar, remover e listar os livros do acervo, respectivamente.

No código principal, criamos uma instância da classe `Biblioteca`. Em seguida, criamos três instâncias da classe `Livro` com diferentes informações. Utilizamos os métodos da classe `Biblioteca` para adicionar esses livros ao acervo, listar todos os livros presentes na biblioteca e remover um livro específico.

Esse código complexo demonstra conceitos como encapsulamento, herança e polimorfismo, além de utilizar estruturas de dados como listas e métodos para manipular essas estruturas. Ele simula um sistema de gerenciamento de biblioteca básico e pode ser expandido para incluir mais funcionalidades e recursos.