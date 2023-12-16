Claro! Aqui está um exemplo de um código complexo em Java que envolve a criação de um sistema de gerenciamento de biblioteca:

```java
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Scanner;

class Livro {
    private String titulo;
    private String autor;
    private int anoPublicacao;
    
    public Livro(String titulo, String autor, int anoPublicacao) {
        this.titulo = titulo;
        this.autor = autor;
        this.anoPublicacao = anoPublicacao;
    }
    
    public String getTitulo() {
        return titulo;
    }
    
    public String getAutor() {
        return autor;
    }
    
    public int getAnoPublicacao() {
        return anoPublicacao;
    }
    
    public String toString() {
        return "Título: " + titulo + ", Autor: " + autor + ", Ano de Publicação: " + anoPublicacao;
    }
}

class Biblioteca {
    private Map<String, List<Livro>> estante;
    
    public Biblioteca() {
        estante = new HashMap<>();
    }
    
    public void adicionarLivro(Livro livro, String genero) {
        if (estante.containsKey(genero)) {
            estante.get(genero).add(livro);
        } else {
            List<Livro> livros = new ArrayList<>();
            livros.add(livro);
            estante.put(genero, livros);
        }
    }
    
    public void removerLivro(String titulo, String genero) {
        if (estante.containsKey(genero)) {
            List<Livro> livros = estante.get(genero);
            for (Livro livro : livros) {
                if (livro.getTitulo().equals(titulo)) {
                    livros.remove(livro);
                    break;
                }
            }
            if (livros.isEmpty()) {
                estante.remove(genero);
            }
        }
    }
    
    public void listarLivros() {
        for (String genero : estante.keySet()) {
            System.out.println("Gênero: " + genero);
            List<Livro> livros = estante.get(genero);
            for (Livro livro : livros) {
                System.out.println(livro);
            }
            System.out.println();
        }
    }
}

public class GerenciamentoBiblioteca {
    public static void main(String[] args) {
        Biblioteca biblioteca = new Biblioteca();
        Scanner scanner = new Scanner(System.in);
        
        System.out.println("Bem-vindo(a) ao sistema de gerenciamento de biblioteca!");
        
        while (true) {
            System.out.println();
            System.out.println("Selecione uma opção:");
            System.out.println("1. Adicionar livro");
            System.out.println("2. Remover livro");
            System.out.println("3. Listar livros");
            System.out.println("0. Sair");
            
            int opcao = scanner.nextInt();
            scanner.nextLine();
            
            if (opcao == 1) {
                System.out.println("Digite o título do livro:");
                String titulo = scanner.nextLine();
                
                System.out.println("Digite o autor do livro:");
                String autor = scanner.nextLine();
                
                System.out.println("Digite o ano de publicação do livro:");
                int anoPublicacao = scanner.nextInt();
                scanner.nextLine();
                
                System.out.println("Digite o gênero do livro:");
                String genero = scanner.nextLine();
                
                Livro livro = new Livro(titulo, autor, anoPublicacao);
                biblioteca.adicionarLivro(livro, genero);
                
                System.out.println("Livro adicionado com sucesso!");
            } else if (opcao == 2) {
                System.out.println("Digite o título do livro:");
                String titulo = scanner.nextLine();
                
                System.out.println("Digite o gênero do livro:");
                String genero = scanner.nextLine();
                
                biblioteca.removerLivro(titulo, genero);
                
                System.out.println("Livro removido com sucesso!");
            } else if (opcao == 3) {
                biblioteca.listarLivros();
            } else if (opcao == 0) {
                System.out.println("Obrigado por utilizar o sistema de gerenciamento de biblioteca!");
                break;
            } else {
                System.out.println("Opção inválida! Por favor, selecione uma opção válida.");
            }
        }
        
        scanner.close();
    }
}
```

Nesse código, temos a classe `Livro` que representa um livro com título, autor e ano de publicação. A classe `Biblioteca` é responsável por gerenciar os livros, permitindo adicionar, remover e listar livros por gênero. A classe `GerenciamentoBiblioteca` é a classe principal que contém o menu interativo para o usuário interagir com a biblioteca.

Ao rodar o programa, o usuário pode selecionar várias opções, como adicionar um livro, remover um livro ou listar todos os livros da biblioteca. O programa é executado em um loop até que o usuário escolha a opção de sair.