Claro! Aqui está um código complexo em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de biblioteca:

```
class Livro {

    atributo titulo : String <- ""
    atributo autor : String <- ""
    atributo disponivel : Bool <- true

    inicializador(new_titulo : String, new_autor : String) : SELF_TYPE {
        titulo <- new_titulo
        autor <- new_autor
        retornar eu
    }

    metodo emprestar() : Object {
        se disponivel então
            disponivel <- false
            retornar self
        senão
            erro "Livro não disponível"
        fim
    }

    metodo devolver() : Object {
        se não disponivel então
            disponivel <- true
            retornar self
        senão
            erro "Livro já foi devolvido"
        fim
    }

    metodo imprimir_detalhes() : Object {
        imprimir "Título: " + titulo
        imprimir "Autor: " + autor
        imprimir "Disponível: " + (se disponivel então "Sim" senão "Não")
        retornar self
    }
}

class Biblioteca {

    atributo livros : List of Livro <- nil

    inicializador() : SELF_TYPE {
        livros <- new List of Livro
        retornar eu
    }

    metodo adicionar_livro(titulo : String, autor : String) : Object {
        livro : Livro <- new Livro inicializador(titulo, autor)
        livros.append(livro)
        retornar self
    }

    metodo emprestar_livro(titulo : String) : Object {
        para cada livro em livros loop
            se livro.titulo = titulo então
                livro.emprestar()
                retornar self
            fim
        fim
        erro "Livro não encontrado"
    }

    metodo devolver_livro(titulo : String) : Object {
        para cada livro em livros loop
            se livro.titulo = titulo então
                livro.devolver()
                retornar self
            fim
        fim
        erro "Livro não encontrado"
    }

    metodo imprimir_livros() : Object {
        imprimir "Livros na biblioteca:"
        para cada livro em livros loop
            livro.imprimir_detalhes()
        fim
        retornar self
    }
}

metodo principal() : Object {
    biblioteca : Biblioteca <- new Biblioteca inicializador()
    biblioteca.adicionar_livro("Dom Casmurro", "Machado de Assis")
    biblioteca.adicionar_livro("O Cortiço", "Aluísio Azevedo")
    biblioteca.adicionar_livro("Grande Sertão: Veredas", "João Guimarães Rosa")

    biblioteca.imprimir_livros()

    biblioteca.emprestar_livro("Dom Casmurro")
    biblioteca.emprestar_livro("O Cortiço")

    biblioteca.imprimir_livros()

    biblioteca.devolver_livro("O Cortiço")

    biblioteca.imprimir_livros()

    retornar self
}
```

Este código em COOL implementa um sistema de gerenciamento de biblioteca com duas classes: `Livro` e `Biblioteca`. 

A classe `Livro` possui atributos `titulo`, `autor` e `disponivel` para representar as informações de um livro. Ela possui métodos `emprestar()`, `devolver()` e `imprimir_detalhes()` para controlar o empréstimo, devolução e exibição dos detalhes de um livro, respectivamente.

A classe `Biblioteca` possui um atributo `livros` para armazenar a lista de livros na biblioteca. Ela possui métodos `adicionar_livro()`, `emprestar_livro()`, `devolver_livro()` e `imprimir_livros()` para adicionar um livro à biblioteca, emprestar/devolver um livro e exibir todos os livros da biblioteca, respectivamente.

No método `principal()`, são criados alguns livros, adicionados à biblioteca, emprestados, devolvidos e, por fim, a lista de livros é exibida.

Espero que este código atenda às suas expectativas!