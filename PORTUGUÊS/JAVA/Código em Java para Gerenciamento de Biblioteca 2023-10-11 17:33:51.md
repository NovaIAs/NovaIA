Claro! Aqui está um código complexo em Java que envolve a criação de um sistema de gerenciamento de biblioteca:

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
  
  public String getTitulo() {
    return titulo;
  }
  
  public String getAutor() {
    return autor;
  }
  
  public int getAnoPublicacao() {
    return anoPublicacao;
  }
  
  @Override
  public String toString() {
    return "Título: " + titulo + ", Autor: " + autor + ", Ano de Publicação: " + anoPublicacao;
  }
}

class Biblioteca {
  private List<Livro> livros;
  
  public Biblioteca() {
    livros = new ArrayList<>();
  }
  
  public void adicionarLivro(Livro livro) {
    livros.add(livro);
  }
  
  public void removerLivro(Livro livro) {
    livros.remove(livro);
  }
  
  public List<Livro> pesquisarPorAutor(String autor) {
    List<Livro> livrosEncontrados = new ArrayList<>();
    
    for (Livro livro : livros) {
      if (livro.getAutor().equalsIgnoreCase(autor)) {
        livrosEncontrados.add(livro);
      }
    }
    
    return livrosEncontrados;
  }
  
  public List<Livro> pesquisarPorTitulo(String titulo) {
    List<Livro> livrosEncontrados = new ArrayList<>();
    
    for (Livro livro : livros) {
      if (livro.getTitulo().equalsIgnoreCase(titulo)) {
        livrosEncontrados.add(livro);
      }
    }
    
    return livrosEncontrados;
  }
  
  public List<Livro> pesquisarPorAno(int ano) {
    List<Livro> livrosEncontrados = new ArrayList<>();
    
    for (Livro livro : livros) {
      if (livro.getAnoPublicacao() == ano) {
        livrosEncontrados.add(livro);
      }
    }
    
    return livrosEncontrados;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    for (Livro livro : livros) {
      sb.append(livro.toString()).append("\n");
    }
    return sb.toString();
  }
}

public class GerenciamentoBiblioteca {
  public static void main(String[] args) {
    Biblioteca biblioteca = new Biblioteca();
    
    Livro livro1 = new Livro("Dom Casmurro", "Machado de Assis", 1899);
    Livro livro2 = new Livro("O Senhor dos Anéis", "J.R.R. Tolkien", 1954);
    Livro livro3 = new Livro("1984", "George Orwell", 1949);
    Livro livro4 = new Livro("O Pequeno Príncipe", "Antoine de Saint-Exupéry", 1943);
    
    biblioteca.adicionarLivro(livro1);
    biblioteca.adicionarLivro(livro2);
    biblioteca.adicionarLivro(livro3);
    biblioteca.adicionarLivro(livro4);
    
    System.out.println("Livros na biblioteca:");
    System.out.println(biblioteca);
    
    List<Livro> livrosEncontrados = biblioteca.pesquisarPorAutor("George Orwell");
    System.out.println("Livros encontrados para o autor 'George Orwell':");
    for (Livro livro : livrosEncontrados) {
      System.out.println(livro);
    }
    
    biblioteca.removerLivro(livro3);
    
    livrosEncontrados = biblioteca.pesquisarPorTitulo("O Senhor dos Anéis");
    System.out.println("Livros encontrados para o título 'O Senhor dos Anéis':");
    for (Livro livro : livrosEncontrados) {
      System.out.println(livro);
    }
  }
}
```

Nesse código, temos três classes principais: `Livro`, `Biblioteca` e `GerenciamentoBiblioteca`.

A classe `Livro` representa um livro com atributos como título, autor e ano de publicação. Ela possui um construtor para inicializar esses atributos e métodos para acessá-los.

A classe `Biblioteca` é responsável pelo gerenciamento dos livros. Ela possui uma lista de livros e métodos para adicionar, remover e pesquisar por livros. A pesquisa pode ser feita por autor, título ou ano de publicação.

A classe `GerenciamentoBiblioteca` é a classe principal que contém o método `main`. Nesse método, criamos uma instância da classe `Biblioteca` e adicionamos alguns livros. Em seguida, imprimimos a lista de livros da biblioteca e fazemos algumas pesquisas por autor e título.

Esse código cria um sistema básico de gerenciamento de biblioteca, permitindo adicionar, remover e pesquisar livros.