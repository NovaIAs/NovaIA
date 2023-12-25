Claro! Vou gerar um código complexo em Java para você. Este código é um sistema de gerenciamento de uma biblioteca, que permite adicionar, pesquisar, emprestar e devolver livros. Ele é dividido em várias classes para uma melhor organização e reutilização de código.

```java
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

class Livro {
    private String titulo;
    private String autor;
    private boolean emprestado;

    public Livro(String titulo, String autor) {
        this.titulo = titulo;
        this.autor = autor;
        this.emprestado = false;
    }

    public String getTitulo() {
        return titulo;
    }

    public String getAutor() {
        return autor;
    }

    public boolean isEmprestado() {
        return emprestado;
    }

    public void setEmprestado(boolean emprestado) {
        this.emprestado = emprestado;
    }
}

class Biblioteca {
    private List<Livro> livros;

    public Biblioteca() {
        this.livros = new ArrayList<>();
    }

    public void adicionarLivro(String titulo, String autor) {
        Livro livro = new Livro(titulo, autor);
        livros.add(livro);
    }

    public void listarLivros() {
        for (Livro livro : livros) {
            System.out.println("Título: " + livro.getTitulo());
            System.out.println("Autor: " + livro.getAutor());
            System.out.println("Emprestado: " + (livro.isEmprestado() ? "Sim" : "Não"));
            System.out.println("---------------------");
        }
    }

    public void pesquisarLivro(String titulo) {
        for (Livro livro : livros) {
            if (livro.getTitulo().equalsIgnoreCase(titulo)) {
                System.out.println("Título: " + livro.getTitulo());
                System.out.println("Autor: " + livro.getAutor());
                System.out.println("Emprestado: " + (livro.isEmprestado() ? "Sim" : "Não"));
                System.out.println("---------------------");
                return;
            }
        }
        System.out.println("Livro não encontrado.");
    }

    public void emprestarLivro(String titulo) {
        for (Livro livro : livros) {
            if (livro.getTitulo().equalsIgnoreCase(titulo)) {
                if (livro.isEmprestado()) {
                    System.out.println("Livro já está emprestado.");
                } else {
                    livro.setEmprestado(true);
                    System.out.println("Livro emprestado com sucesso.");
                }
                return;
            }
        }
        System.out.println("Livro não encontrado.");
    }

    public void devolverLivro(String titulo) {
        for (Livro livro : livros) {
            if (livro.getTitulo().equalsIgnoreCase(titulo)) {
                if (livro.isEmprestado()) {
                    livro.setEmprestado(false);
                    System.out.println("Livro devolvido com sucesso.");
                } else {
                    System.out.println("Livro já está disponível na biblioteca.");
                }
                return;
            }
        }
        System.out.println("Livro não encontrado.");
    }
}

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        Biblioteca biblioteca = new Biblioteca();

        while (true) {
            System.out.println("Sistema de Biblioteca");
            System.out.println("1. Adicionar livro");
            System.out.println("2. Listar livros");
            System.out.println("3. Pesquisar livro");
            System.out.println("4. Emprestar livro");
            System.out.println("5. Devolver livro");
            System.out.println("0. Sair");
            System.out.print("Escolha uma opção: ");
            int opcao = scanner.nextInt();
            scanner.nextLine(); // Limpar o buffer

            switch (opcao) {
                case 1:
                    System.out.print("Informe o título do livro: ");
                    String titulo = scanner.nextLine();
                    System.out.print("Informe o autor do livro: ");
                    String autor = scanner.nextLine();
                    biblioteca.adicionarLivro(titulo, autor);
                    System.out.println("Livro adicionado com sucesso.");
                    break;
                case 2:
                    biblioteca.listarLivros();
                    break;
                case 3:
                    System.out.print("Informe o título do livro: ");
                    titulo = scanner.nextLine();
                    biblioteca.pesquisarLivro(titulo);
                    break;
                case 4:
                    System.out.print("Informe o título do livro: ");
                    titulo = scanner.nextLine();
                    biblioteca.emprestarLivro(titulo);
                    break;
                case 5:
                    System.out.print("Informe o título do livro: ");
                    titulo = scanner.nextLine();
                    biblioteca.devolverLivro(titulo);
                    break;
                case 0:
                    System.out.println("Saindo do sistema...");
                    System.exit(0);
                    break;
                default:
                    System.out.println("Opção inválida.");
                    break;
            }
            System.out.println();
        }
    }
}
```

Este código cria uma classe `Livro` que representa um livro com título, autor e um indicador se está emprestado ou não. A classe `Biblioteca` é responsável por gerenciar os livros, permitindo adicionar, listar, pesquisar, emprestar e devolver livros. A classe `Main` é a classe principal que contém o menu interativo para interagir com o sistema de biblioteca.

Espero que este código atenda às suas expectativas!