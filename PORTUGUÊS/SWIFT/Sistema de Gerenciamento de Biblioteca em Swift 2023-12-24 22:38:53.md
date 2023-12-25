Claro! Aqui está um código complexo em Swift. Este código é um sistema de gerenciamento de biblioteca com a capacidade de adicionar, remover e pesquisar livros. Ele utiliza conceitos como classes, herança, estruturas de dados e manipulação de arrays. Vou explicar cada parte do código para facilitar o entendimento:

```swift
// Definindo a estrutura Livro, que contém as propriedades título e autor
struct Livro {
    var titulo: String
    var autor: String
}

// Definindo a classe Biblioteca, responsável por gerenciar os livros
class Biblioteca {
    var livros: [Livro] = []
    
    // Método para adicionar um livro à biblioteca
    func adicionarLivro(titulo: String, autor: String) {
        let livro = Livro(titulo: titulo, autor: autor)
        livros.append(livro)
        print("Livro adicionado com sucesso!")
    }
    
    // Método para remover um livro da biblioteca
    func removerLivro(titulo: String) {
        if let index = livros.firstIndex(where: { $0.titulo == titulo }) {
            livros.remove(at: index)
            print("Livro removido com sucesso!")
        } else {
            print("Livro não encontrado na biblioteca.")
        }
    }
    
    // Método para pesquisar um livro na biblioteca
    func pesquisarLivro(titulo: String) {
        if let livro = livros.first(where: { $0.titulo == titulo }) {
            print("Livro encontrado:")
            print("Título: \(livro.titulo)")
            print("Autor: \(livro.autor)")
        } else {
            print("Livro não encontrado na biblioteca.")
        }
    }
}

// Exemplo de uso da biblioteca
let biblioteca = Biblioteca()

// Adicionando alguns livros à biblioteca
biblioteca.adicionarLivro(titulo: "Dom Casmurro", autor: "Machado de Assis")
biblioteca.adicionarLivro(titulo: "O Senhor dos Anéis", autor: "J.R.R. Tolkien")

// Pesquisando um livro na biblioteca
biblioteca.pesquisarLivro(titulo: "Dom Casmurro")

// Removendo um livro da biblioteca
biblioteca.removerLivro(titulo: "O Senhor dos Anéis")

// Pesquisando novamente o livro removido
biblioteca.pesquisarLivro(titulo: "O Senhor dos Anéis")
```

Neste código, primeiro definimos a estrutura `Livro`, que possui as propriedades `titulo` e `autor`. Em seguida, criamos a classe `Biblioteca`, que possui uma propriedade `livros` que é um array de `Livro`. 

Dentro da classe `Biblioteca`, temos três métodos:

1. O método `adicionarLivro` recebe o título e autor de um livro, cria uma instância de `Livro` com esses dados e adiciona o livro ao array `livros`.
2. O método `removerLivro` recebe o título de um livro, verifica se ele está presente no array `livros` e, caso esteja, remove-o.
3. O método `pesquisarLivro` recebe o título de um livro, verifica se ele está presente no array `livros` e, caso esteja, exibe suas informações (título e autor).

No exemplo de uso da biblioteca, criamos uma instância de `Biblioteca`, adicionamos alguns livros, pesquisamos um livro, removemos outro livro e pesquisamos novamente o livro removido.

Espero que este código atenda às suas expectativas!