Claro! Aqui está um exemplo de um código complexo em Java que implementa uma simulação de um sistema de gerenciamento de uma biblioteca. Ele possui classes para representar os livros, os usuários e as transações realizadas, além de métodos para realizar operações como empréstimos, devoluções e consultas.

```java
import java.util.ArrayList;
import java.util.List;

class Livro {
    private int id;
    private String titulo;
    private String autor;
    private boolean disponivel;

    public Livro(int id, String titulo, String autor) {
        this.id = id;
        this.titulo = titulo;
        this.autor = autor;
        this.disponivel = true;
    }

    public int getId() {
        return id;
    }

    public String getTitulo() {
        return titulo;
    }

    public String getAutor() {
        return autor;
    }

    public boolean isDisponivel() {
        return disponivel;
    }

    public void emprestar() {
        this.disponivel = false;
    }

    public void devolver() {
        this.disponivel = true;
    }
}

class Usuario {
    private int id;
    private String nome;
    private List<Livro> livrosEmprestados;

    public Usuario(int id, String nome) {
        this.id = id;
        this.nome = nome;
        this.livrosEmprestados = new ArrayList<>();
    }

    public int getId() {
        return id;
    }

    public String getNome() {
        return nome;
    }

    public List<Livro> getLivrosEmprestados() {
        return livrosEmprestados;
    }

    public void emprestarLivro(Livro livro) {
        if (livro.isDisponivel()) {
            livro.emprestar();
            livrosEmprestados.add(livro);
            System.out.println("Livro emprestado com sucesso!");
        } else {
            System.out.println("Livro indisponível para empréstimo.");
        }
    }

    public void devolverLivro(Livro livro) {
        if (livrosEmprestados.contains(livro)) {
            livro.devolver();
            livrosEmprestados.remove(livro);
            System.out.println("Livro devolvido com sucesso!");
        } else {
            System.out.println("Livro não está em posse do usuário.");
        }
    }
}

class Transacao {
    private String tipo;
    private Livro livro;
    private Usuario usuario;

    public Transacao(String tipo, Livro livro, Usuario usuario) {
        this.tipo = tipo;
        this.livro = livro;
        this.usuario = usuario;
    }

    public String getTipo() {
        return tipo;
    }

    public Livro getLivro() {
        return livro;
    }

    public Usuario getUsuario() {
        return usuario;
    }
}

public class Biblioteca {
    private List<Livro> livros;
    private List<Usuario> usuarios;
    private List<Transacao> historicoTransacoes;

    public Biblioteca() {
        this.livros = new ArrayList<>();
        this.usuarios = new ArrayList<>();
        this.historicoTransacoes = new ArrayList<>();
    }

    public void adicionarLivro(Livro livro) {
        livros.add(livro);
        System.out.println("Livro adicionado à biblioteca.");
    }

    public void removerLivro(Livro livro) {
        if (livros.contains(livro)) {
            livros.remove(livro);
            System.out.println("Livro removido da biblioteca.");
        } else {
            System.out.println("Livro não encontrado na biblioteca.");
        }
    }

    public void adicionarUsuario(Usuario usuario) {
        usuarios.add(usuario);
        System.out.println("Usuário adicionado à biblioteca.");
    }

    public void removerUsuario(Usuario usuario) {
        if (usuarios.contains(usuario)) {
            usuarios.remove(usuario);
            System.out.println("Usuário removido da biblioteca.");
        } else {
            System.out.println("Usuário não encontrado na biblioteca.");
        }
    }

    public void realizarEmprestimo(Usuario usuario, Livro livro) {
        if (usuarios.contains(usuario) && livros.contains(livro)) {
            usuario.emprestarLivro(livro);
            historicoTransacoes.add(new Transacao("Empréstimo", livro, usuario));
        } else {
            System.out.println("Usuário ou livro não encontrado na biblioteca.");
        }
    }

    public void realizarDevolucao(Usuario usuario, Livro livro) {
        if (usuarios.contains(usuario) && livros.contains(livro)) {
            usuario.devolverLivro(livro);
            historicoTransacoes.add(new Transacao("Devolução", livro, usuario));
        } else {
            System.out.println("Usuário ou livro não encontrado na biblioteca.");
        }
    }

    public void consultarLivrosDisponiveis() {
        System.out.println("Livros disponíveis na biblioteca:");
        for (Livro livro : livros) {
            if (livro.isDisponivel()) {
                System.out.println("ID: " + livro.getId() + " | Título: " + livro.getTitulo() + " | Autor: " + livro.getAutor());
            }
        }
    }

    public void consultarLivrosEmprestados(Usuario usuario) {
        System.out.println("Livros em posse do usuário " + usuario.getNome() + ":");
        for (Livro livro : usuario.getLivrosEmprestados()) {
            System.out.println("ID: " + livro.getId() + " | Título: " + livro.getTitulo() + " | Autor: " + livro.getAutor());
        }
    }

    public void consultarHistoricoTransacoes() {
        System.out.println("Histórico de transações:");
        for (Transacao transacao : historicoTransacoes) {
            System.out.println("Tipo: " + transacao.getTipo() + " | Livro: " + transacao.getLivro().getTitulo() +
                    " | Usuário: " + transacao.getUsuario().getNome());
        }
    }

    public static void main(String[] args) {
        Biblioteca biblioteca = new Biblioteca();

        Livro livro1 = new Livro(1, "Dom Casmurro", "Machado de Assis");
        Livro livro2 = new Livro(2, "O Senhor dos Anéis", "J.R.R. Tolkien");
        Livro livro3 = new Livro(3, "1984", "George Orwell");

        biblioteca.adicionarLivro(livro1);
        biblioteca.adicionarLivro(livro2);
        biblioteca.adicionarLivro(livro3);

        Usuario usuario1 = new Usuario(1, "João");
        Usuario usuario2 = new Usuario(2, "Maria");

        biblioteca.adicionarUsuario(usuario1);
        biblioteca.adicionarUsuario(usuario2);

        biblioteca.realizarEmprestimo(usuario1, livro1);
        biblioteca.realizarEmprestimo(usuario2, livro2);

        biblioteca.consultarLivrosDisponiveis();
        biblioteca.consultarLivrosEmprestados(usuario1);

        biblioteca.realizarDevolucao(usuario1, livro1);

        biblioteca.consultarHistoricoTransacoes();
    }
}
```

Neste código, temos três classes principais: `Livro`, `Usuario` e `Transacao`. A classe `Livro` representa um livro, contendo atributos como ID, título, autor e um indicador de disponibilidade. A classe `Usuario` representa um usuário da biblioteca, contendo ID, nome e uma lista de livros emprestados. A classe `Transacao` representa uma transação, contendo o tipo (empréstimo ou devolução), o livro envolvido e o usuário envolvido.

A classe `Biblioteca` é a classe principal que contém os métodos para adicionar e remover livros e usuários, realizar empréstimos, devoluções, consultas de livros disponíveis, livros emprestados e histórico de transações.

No método `main`, criamos uma instância da classe `Biblioteca` e alguns livros e usuários de exemplo. Em seguida, realizamos algumas operações como adicionar livros e usuários, realizar empréstimos, fazer consultas e realizar devoluções. No final, exibimos o histórico de transações.

Espero que esse código atenda às suas necessidades. Se você tiver alguma dúvida, estou à disposição para ajudar!