Claro! Aqui está um exemplo de um código complexo em Groovy que implementa um sistema de gerenciamento de biblioteca:

```groovy
import java.util.Scanner

// Definindo a classe Livro
class Livro {
    String titulo
    String autor
    int anoPublicacao
    
    // Construtor da classe Livro
    Livro(String titulo, String autor, int anoPublicacao) {
        this.titulo = titulo
        this.autor = autor
        this.anoPublicacao = anoPublicacao
    }
    
    // Método para exibir informações sobre o livro
    void exibir() {
        println("Título: ${titulo}")
        println("Autor: ${autor}")
        println("Ano de publicação: ${anoPublicacao}")
        println("-----")
    }
}

// Definindo a classe Biblioteca
class Biblioteca {
    ArrayList<Livro> acervo = new ArrayList<Livro>()
    
    // Método para adicionar um livro ao acervo da biblioteca
    void adicionarLivro(Livro livro) {
        acervo.add(livro)
    }
    
    // Método para exibir todos os livros do acervo
    void exibirAcervo() {
        if (acervo.size() == 0) {
            println("A biblioteca está vazia.")
        } else {
            println("Acervo da biblioteca:")
            for (Livro livro : acervo) {
                livro.exibir()
            }
        }
    }
    
    // Método para buscar livros por autor
    void buscarPorAutor(String autor) {
        boolean encontrouLivro = false
        
        for (Livro livro : acervo) {
            if (livro.autor.equalsIgnoreCase(autor)) {
                livro.exibir()
                encontrouLivro = true
            }
        }
        
        if (!encontrouLivro) {
            println("Nenhum livro encontrado para o autor informado.")
        }
    }
    
    // Método para buscar livros por ano de publicação
    void buscarPorAnoPublicacao(int ano) {
        boolean encontrouLivro = false
        
        for (Livro livro : acervo) {
            if (livro.anoPublicacao == ano) {
                livro.exibir()
                encontrouLivro = true
            }
        }
        
        if (!encontrouLivro) {
            println("Nenhum livro encontrado para o ano de publicação informado.")
        }
    }
}

// Programa principal
def biblioteca = new Biblioteca()

def opcao = 0

while (opcao != 5) {
    println("----- MENU -----")
    println("1. Adicionar livro")
    println("2. Exibir acervo")
    println("3. Buscar por autor")
    println("4. Buscar por ano de publicação")
    println("5. Sair")
    
    def scanner = new Scanner(System.in)
    opcao = scanner.nextInt()
    
    switch (opcao) {
        case 1:
            println("Informe o título do livro:")
            def titulo = scanner.next()
            
            println("Informe o autor do livro:")
            def autor = scanner.next()
            
            println("Informe o ano de publicação do livro:")
            def anoPublicacao = scanner.nextInt()
            
            def livro = new Livro(titulo, autor, anoPublicacao)
            biblioteca.adicionarLivro(livro)
            println("Livro adicionado com sucesso!")
            break
        case 2:
            biblioteca.exibirAcervo()
            break
        case 3:
            println("Informe o autor para buscar:")
            def autorBusca = scanner.next()
            biblioteca.buscarPorAutor(autorBusca)
            break
        case 4:
            println("Informe o ano de publicação para buscar:")
            def anoBusca = scanner.nextInt()
            biblioteca.buscarPorAnoPublicacao(anoBusca)
            break
        case 5:
            println("Saindo do programa...")
            break
        default:
            println("Opção inválida. Por favor, tente novamente.")
            break
    }
}
```

Este código em Groovy implementa um sistema de gerenciamento de biblioteca. Ele permite adicionar livros ao acervo da biblioteca, exibir todos os livros do acervo, buscar livros por autor e buscar livros por ano de publicação.

A classe `Livro` define os atributos de um livro (título, autor e ano de publicação) e possui um método `exibir()` para exibir as informações do livro.

A classe `Biblioteca` possui um atributo `acervo` que é uma lista de livros. Ela possui métodos para adicionar livros ao acervo, exibir os livros do acervo, buscar livros por autor e buscar livros por ano de publicação.

O programa principal utiliza um loop para exibir um menu de opções ao usuário. Ele lê a opção escolhida pelo usuário e executa a ação correspondente. O programa continua executando até que o usuário escolha a opção para sair.

Esse é apenas um exemplo de código complexo em Groovy, mas existem muitas outras possibilidades e variações de códigos que podem ser criados.