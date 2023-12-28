Claro! Aqui está um código em Swift que cria um sistema de gerenciamento de biblioteca:

```swift
import Foundation

class Livro {
    let titulo: String
    let autor: String
    var disponivel: Bool
    
    init(titulo: String, autor: String) {
        self.titulo = titulo
        self.autor = autor
        self.disponivel = true
    }
}

class Biblioteca {
    var livros: [Livro]
    
    init() {
        self.livros = []
    }
    
    func adicionarLivro(_ livro: Livro) {
        livros.append(livro)
    }
    
    func removerLivro(_ livro: Livro) {
        livros = livros.filter { $0.titulo != livro.titulo }
    }
    
    func buscarLivroPorTitulo(_ titulo: String) -> Livro? {
        for livro in livros {
            if livro.titulo.lowercased() == titulo.lowercased() {
                return livro
            }
        }
        return nil
    }
    
    func listarLivrosDisponiveis() {
        for livro in livros {
            if livro.disponivel {
                print("Título: \(livro.titulo), Autor: \(livro.autor)")
            }
        }
    }
}

// Exemplo de uso do código:

let biblioteca = Biblioteca()

let livro1 = Livro(titulo: "Dom Casmurro", autor: "Machado de Assis")
let livro2 = Livro(titulo: "1984", autor: "George Orwell")
let livro3 = Livro(titulo: "A Revolução dos Bichos", autor: "George Orwell")

biblioteca.adicionarLivro(livro1)
biblioteca.adicionarLivro(livro2)
biblioteca.adicionarLivro(livro3)

biblioteca.listarLivrosDisponiveis()

let livroBuscado = biblioteca.buscarLivroPorTitulo("1984")
if let livro = livroBuscado {
    print("Livro encontrado: \(livro.titulo), \(livro.autor)")
} else {
    print("Livro não encontrado.")
}

biblioteca.removerLivro(livro2)

biblioteca.listarLivrosDisponiveis()
```

Neste código, temos duas classes principais: `Livro` e `Biblioteca`. A classe `Livro` representa um livro com atributos como título, autor e disponibilidade. Já a classe `Biblioteca` representa a biblioteca, com uma lista de livros e métodos para adicionar, remover, buscar por título e listar os livros disponíveis.

No exemplo de uso do código, criamos uma instância da biblioteca e alguns livros. Em seguida, adicionamos os livros à biblioteca e listamos os livros disponíveis. Fazemos uma busca por um livro específico e, caso encontrado, exibimos suas informações. Por fim, removemos um livro da biblioteca e listamos novamente os livros disponíveis.