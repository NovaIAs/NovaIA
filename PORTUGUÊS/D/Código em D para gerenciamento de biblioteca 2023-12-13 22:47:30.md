Claro! Vou gerar um código complexo em D para você. Este código será um programa de gerenciamento de uma biblioteca. Ele terá várias funcionalidades, como cadastro de livros e usuários, empréstimos, devoluções, entre outros. Vou explicar o código passo a passo para facilitar o entendimento.

```d
import std.stdio;
import std.algorithm;
import std.container;

struct Livro {
    string titulo;
    string autor;
    int anoPublicacao;
    bool emprestado;
}

struct Usuario {
    string nome;
    int idade;
    int numLivrosEmprestados;
}

class Biblioteca {
    Livro[] livros;
    Usuario[] usuarios;

    this() {
        livros = [];
        usuarios = [];
    }

    void cadastrarLivro(string titulo, string autor, int anoPublicacao) {
        Livro livro;
        livro.titulo = titulo;
        livro.autor = autor;
        livro.anoPublicacao = anoPublicacao;
        livro.emprestado = false;
        livros ~= livro;
    }

    void cadastrarUsuario(string nome, int idade) {
        Usuario usuario;
        usuario.nome = nome;
        usuario.idade = idade;
        usuario.numLivrosEmprestados = 0;
        usuarios ~= usuario;
    }

    void emprestarLivro(string titulo, string nomeUsuario) {
        foreach (ref livro; livros) {
            if (livro.titulo == titulo && !livro.emprestado) {
                foreach (ref usuario; usuarios) {
                    if (usuario.nome == nomeUsuario && usuario.numLivrosEmprestados < 3) {
                        livro.emprestado = true;
                        usuario.numLivrosEmprestados++;
                        writeln("Livro emprestado com sucesso!");
                        return;
                    }
                }
            }
        }
        writeln("Não foi possível emprestar o livro.");
    }

    void devolverLivro(string titulo, string nomeUsuario) {
        foreach (ref livro; livros) {
            if (livro.titulo == titulo && livro.emprestado) {
                foreach (ref usuario; usuarios) {
                    if (usuario.nome == nomeUsuario && usuario.numLivrosEmprestados > 0) {
                        livro.emprestado = false;
                        usuario.numLivrosEmprestados--;
                        writeln("Livro devolvido com sucesso!");
                        return;
                    }
                }
            }
        }
        writeln("Não foi possível devolver o livro.");
    }

    void listarLivros() {
        foreach (livro; livros) {
            writeln("Título: ", livro.titulo);
            writeln("Autor: ", livro.autor);
            writeln("Ano de Publicação: ", livro.anoPublicacao);
            writeln("Disponível: ", livro.emprestado ? "Não" : "Sim");
            writeln();
        }
    }

    void listarUsuarios() {
        foreach (usuario; usuarios) {
            writeln("Nome: ", usuario.nome);
            writeln("Idade: ", usuario.idade);
            writeln("Número de Livros Emprestados: ", usuario.numLivrosEmprestados);
            writeln();
        }
    }
}

void main() {
    Biblioteca biblioteca = new Biblioteca();

    // Cadastrando livros
    biblioteca.cadastrarLivro("Dom Quixote", "Miguel de Cervantes", 1605);
    biblioteca.cadastrarLivro("Grande Sertão: Veredas", "João Guimarães Rosa", 1956);
    biblioteca.cadastrarLivro("Cem Anos de Solidão", "Gabriel García Márquez", 1967);

    // Cadastrando usuários
    biblioteca.cadastrarUsuario("João", 25);
    biblioteca.cadastrarUsuario("Maria", 30);
    biblioteca.cadastrarUsuario("Pedro", 18);

    // Emprestando livros
    biblioteca.emprestarLivro("Dom Quixote", "João");
    biblioteca.emprestarLivro("Grande Sertão: Veredas", "Maria");
    biblioteca.emprestarLivro("Cem Anos de Solidão", "Pedro");
    biblioteca.emprestarLivro("Cem Anos de Solidão", "Maria"); // Não é possível emprestar para o mesmo usuário

    // Devolvendo livros
    biblioteca.devolverLivro("Dom Quixote", "João");
    biblioteca.devolverLivro("Grande Sertão: Veredas", "Maria");
    biblioteca.devolverLivro("Cem Anos de Solidão", "Pedro");
    biblioteca.devolverLivro("Cem Anos de Solidão", "Maria"); // Não é possível devolver se o livro não estiver emprestado

    // Listando livros e usuários
    biblioteca.listarLivros();
    biblioteca.listarUsuarios();
}
```

Este é um exemplo de um código complexo em D que simula o gerenciamento de uma biblioteca. Ele utiliza estruturas de dados, como structs para representar Livro e Usuario, e uma class Biblioteca para gerenciar esses dados.

A classe Biblioteca contém métodos para cadastrar livros e usuários, emprestar e devolver livros, e também para listar os livros e usuários cadastrados. O código principal no final do programa demonstra como utilizar esses métodos.

O programa começa criando uma instância da classe Biblioteca e em seguida cadastra alguns livros e usuários utilizando os métodos `cadastrarLivro()` e `cadastrarUsuario()`. Em seguida, são realizados empréstimos e devoluções de livros utilizando os métodos `emprestarLivro()` e `devolverLivro()`. Por fim, são listados os livros e usuários cadastrados utilizando os métodos `listarLivros()` e `listarUsuarios()`.

Espero que este código complexo em D atenda às suas expectativas!