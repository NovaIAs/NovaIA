Claro! Aqui está um código em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de biblioteca.

```
class Livro {
    atributo titulo : String <- ""
    atributo autor : String <- ""
    atributo disponivel : Bool <- true
    
    metodo inicializar(t : String, a : String) : SELF_TYPE {
        titulo <- t
        autor <- a
        retorne eu
    }
    
    metodo emprestar() : Bool {
        se disponivel então
            disponivel <- falso
            retorne verdadeiro
        senão
            retorne falso
    }
    
    metodo devolver() : Bool {
        se não disponivel então
            disponivel <- verdadeiro
            retorne verdadeiro
        senão
            retorne falso
    }
    
    metodo toString() : String {
        retorne "Título: " + titulo + ", Autor: " + autor + ", Disponível: " + disponivel.comoTexto
    }
}

class Biblioteca {
    atributo livros : List of Livro <- nil
    
    metodo adicionarLivro(l : Livro) : SELF_TYPE {
        se livros = nil então
            livros <- new List of Livro
        fim
        livros.insiraNoFim(l)
        retorne eu
    }
    
    metodo removerLivro(l : Livro) : SELF_TYPE {
        livros.remova(l)
        retorne eu
    }
    
    metodo listarLivros() : String {
        resultado : String <- ""
        para cada livro em livros faça
            resultado <- resultado + livro.toString() + "\n"
        fim
        retorne resultado
    }
}

class Main {
    metodo principal() : Object {
        biblioteca : Biblioteca <- new Biblioteca
        
        livro1 : Livro <- new Livro.inicializar("Dom Casmurro", "Machado de Assis")
        livro2 : Livro <- new Livro.inicializar("O Pequeno Príncipe", "Antoine de Saint-Exupéry")
        
        biblioteca.adicionarLivro(livro1)
        biblioteca.adicionarLivro(livro2)
        
        biblioteca.listarLivros().imprima()
        
        livro1.emprestar()
        livro2.emprestar()
        
        biblioteca.listarLivros().imprima()
        
        livro1.devolver()
        
        biblioteca.listarLivros().imprima()
        
        livro2.devolver()
        
        biblioteca.listarLivros().imprima()
        
        retorne nulo
    }
}
```

Neste código, temos três classes: `Livro`, `Biblioteca` e `Main`.

A classe `Livro` possui os atributos `titulo`, `autor` e `disponivel`, que representam informações sobre um livro. Ela também possui os métodos `inicializar`, `emprestar`, `devolver` e `toString`. O método `inicializar` recebe o título e o autor do livro como parâmetros e os atribui aos atributos correspondentes. O método `emprestar` altera o estado do livro para indisponível se o livro estiver disponível, e retorna verdadeiro. Caso contrário, retorna falso. O método `devolver` altera o estado do livro para disponível se o livro não estiver disponível, e retorna verdadeiro. Caso contrário, retorna falso. O método `toString` retorna uma representação em forma de string do livro, contendo o título, autor e disponibilidade.

A classe `Biblioteca` possui o atributo `livros`, que é uma lista de objetos do tipo `Livro`. Ela também possui os métodos `adicionarLivro`, `removerLivro` e `listarLivros`. O método `adicionarLivro` recebe um objeto `Livro` como parâmetro e adiciona-o à lista de livros da biblioteca. O método `removerLivro` recebe um objeto `Livro` como parâmetro e remove-o da lista de livros. O método `listarLivros` retorna uma string contendo a representação de todos os livros da biblioteca.

A classe `Main` é a classe principal do programa. Ela cria uma instância da classe `Biblioteca` e duas instâncias da classe `Livro`, inicializa-os com informações e os adiciona à biblioteca. Em seguida, chama o método `listarLivros` da biblioteca para exibir os livros adicionados. Depois, empresta e devolve um dos livros e chama o método `listarLivros` novamente para mostrar as alterações.

Esse código é apenas um exemplo de implementação de um sistema de gerenciamento de biblioteca em COOL e pode ser personalizado e expandido de acordo com as necessidades específicas de um projeto real.