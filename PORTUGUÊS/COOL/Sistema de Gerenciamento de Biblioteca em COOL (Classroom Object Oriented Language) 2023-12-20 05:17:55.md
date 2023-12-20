Certamente, vou gerar um código em COOL (Classroom Object Oriented Language) para um sistema de gerenciamento de biblioteca em português. Este código é um exemplo de um projeto complexo e diferenciado que envolve várias classes e funcionalidades.

```
class Livro {
    string titulo
    string autor
    int anoPublicacao
    float preco
    int quantidadeEstoque

    metodos {
        // Construtor
        public novoLivro(titulo: string, autor: string, anoPublicacao: int, preco: float, quantidadeEstoque: int) {
            this.titulo <- titulo
            this.autor <- autor
            this.anoPublicacao <- anoPublicacao
            this.preco <- preco
            this.quantidadeEstoque <- quantidadeEstoque
        }

        // Método para atualizar informações de um livro
        public atualizarLivro(titulo: string, autor: string, anoPublicacao: int, preco: float, quantidadeEstoque: int) {
            this.titulo <- titulo
            this.autor <- autor
            this.anoPublicacao <- anoPublicacao
            this.preco <- preco
            this.quantidadeEstoque <- quantidadeEstoque
        }
    }
}

class Biblioteca {
    listaLivros: array of Livro

    metodos {
        // Adicionar um novo livro à biblioteca
        public adicionarLivro(titulo: string, autor: string, anoPublicacao: int, preco: float, quantidadeEstoque: int) {
            novoLivro: Livro <- Livro().novoLivro(titulo, autor, anoPublicacao, preco, quantidadeEstoque)
            listaLivros <- listaLivros + [novoLivro]
        }

        // Remover um livro da biblioteca
        public removerLivro(livro: Livro) {
            listaLivros <- listaLivros - [livro]
        }

        // Buscar livros pelo título
        public buscarPorTitulo(titulo: string): array of Livro {
            resultado: array of Livro <- []

            for livro in listaLivros {
                if (livro.titulo = titulo) {
                    resultado <- resultado + [livro]
                }
            }

            return resultado
        }

        // Buscar livros pelo autor
        public buscarPorAutor(autor: string): array of Livro {
            resultado: array of Livro <- []

            for livro in listaLivros {
                if (livro.autor = autor) {
                    resultado <- resultado + [livro]
                }
            }

            return resultado
        }

        // Listar todos os livros disponíveis na biblioteca
        public listarLivros() {
            for livro in listaLivros {
                print("Título: " + livro.titulo)
                print("Autor: " + livro.autor)
                print("Ano de publicação: " + livro.anoPublicacao)
                print("Preço: " + livro.preco)
                print("Quantidade em estoque: " + livro.quantidadeEstoque)
                print("----------------------------")
            }
        }
    }
}

// Exemplo de uso do sistema de gerenciamento de biblioteca
biblioteca: Biblioteca <- Biblioteca()
biblioteca.adicionarLivro("Dom Casmurro", "Machado de Assis", 1899, 29.90, 5)
biblioteca.adicionarLivro("A Arte da Guerra", "Sun Tzu", 500 a.C., 19.90, 2)
biblioteca.adicionarLivro("O Pequeno Príncipe", "Antoine de Saint-Exupéry", 1943, 24.90, 10)

// Buscar e listar livros pelo título
livrosEncontrados: array of Livro <- biblioteca.buscarPorTitulo("O Pequeno Príncipe")
biblioteca.listarLivros()

// Remover um livro da biblioteca
livroRemovido: Livro <- livrosEncontrados[0]
biblioteca.removerLivro(livroRemovido)
biblioteca.listarLivros()
```

Neste código, temos duas classes principais: `Livro` e `Biblioteca`. A classe `Livro` representa um livro com atributos como título, autor, ano de publicação, preço e quantidade em estoque. A classe `Biblioteca` é responsável por gerenciar uma lista de livros e oferece funcionalidades como adicionar, remover, buscar e listar livros.

O exemplo de uso ao final cria uma instância da classe `Biblioteca`, adiciona alguns livros, busca livros pelo título, remove um livro e lista todos os livros disponíveis na biblioteca.

Este é apenas um exemplo de código complexo em COOL, mas você pode adaptá-lo e adicionar mais funcionalidades de acordo com suas necessidades específicas.