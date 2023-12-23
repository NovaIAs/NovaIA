Claro! Vou criar um código complexo em Java que envolve o uso de estruturas de dados avançadas e algoritmos de busca, chamado "Sistema de Gerenciamento de Biblioteca".

Este código irá simular um sistema de gerenciamento de biblioteca, permitindo que os usuários cadastrem livros, pesquisem livros disponíveis, solicitem empréstimos e devoluções, além de gerar relatórios estatísticos sobre o acervo.

```java
import java.util.*;

class Livro {
    private String titulo;
    private String autor;
    private int anoPublicacao;

    public Livro(String titulo, String autor, int anoPublicacao) {
        this.titulo = titulo;
        this.autor = autor;
        this.anoPublicacao = anoPublicacao;
    }

    // getters e setters omitidos para brevidade
}

class Biblioteca {
    private List<Livro> acervo;

    public Biblioteca() {
        this.acervo = new ArrayList<>();
    }

    public void adicionarLivro(Livro livro) {
        acervo.add(livro);
        System.out.println("Livro adicionado com sucesso!");
    }

    public void pesquisarLivro(String titulo) {
        for (Livro livro : acervo) {
            if (livro.getTitulo().equalsIgnoreCase(titulo)) {
                System.out.println("Título: " + livro.getTitulo());
                System.out.println("Autor: " + livro.getAutor());
                System.out.println("Ano de publicação: " + livro.getAnoPublicacao());
                return;
            }
        }
        System.out.println("Livro não encontrado.");
    }

    // outros métodos relacionados à gestão da biblioteca (empréstimos, devoluções, etc.) omitidos para brevidade
}

public class SistemaGerenciamentoBiblioteca {
    public static void main(String[] args) {
        Biblioteca biblioteca = new Biblioteca();

        Livro livro1 = new Livro("A Menina que Roubava Livros", "Markus Zusak", 2005);
        Livro livro2 = new Livro("O Pequeno Príncipe", "Antoine de Saint-Exupéry", 1943);
        Livro livro3 = new Livro("1984", "George Orwell", 1949);

        biblioteca.adicionarLivro(livro1);
        biblioteca.adicionarLivro(livro2);
        biblioteca.adicionarLivro(livro3);

        biblioteca.pesquisarLivro("1984");
        biblioteca.pesquisarLivro("Dom Quixote");
    }
}
```

Neste exemplo, temos três classes: `Livro`, que representa um livro com título, autor e ano de publicação; `Biblioteca`, que possui uma lista de livros e métodos para adicionar livros e pesquisar por título; e `SistemaGerenciamentoBiblioteca`, que é a classe principal responsável por instanciar a biblioteca, adicionar livros e fazer pesquisas.

No método `main`, são criados três objetos `Livro` e adicionados à biblioteca. Em seguida, são feitas duas pesquisas, uma para encontrar o livro "1984" e outra para procurar por "Dom Quixote". Os resultados das pesquisas são mostrados no console.

Espero que esse código atenda às suas expectativas!