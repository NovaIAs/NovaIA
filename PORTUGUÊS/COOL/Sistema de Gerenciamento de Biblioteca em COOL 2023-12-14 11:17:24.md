Claro! Vou criar um código complexo em COOL (Classroom Object-Oriented Language) para você. Este código será um exemplo de um sistema de gerenciamento de uma biblioteca. Ele terá classes para livros, usuários e empréstimos, além de métodos para adicionar e remover livros, usuários e empréstimos, bem como consultas para verificar a disponibilidade de livros e empréstimos ativos. Vamos começar:

```
class Livro {
    string titulo;
    int anoPublicacao;
    bool disponivel;

    // Construtor da classe Livro
    init(t: string, a: int) {
        titulo <- t;
        anoPublicacao <- a;
        disponivel <- true;
    };

    // Método para verificar a disponibilidade do livro
    bool estaDisponivel(): bool {
        return disponivel;
    };
};

class Usuario {
    string nome;
    int idade;

    // Construtor da classe Usuario
    init(n: string, i: int) {
        nome <- n;
        idade <- i;
    };
};

class Emprestimo {
    Livro livro;
    Usuario usuario;
    date dataEmprestimo;
    date dataDevolucao;

    // Construtor da classe Emprestimo
    init(l: Livro, u: Usuario) {
        livro <- l;
        usuario <- u;
        dataEmprestimo <- getCurrentDate();
        dataDevolucao <- null;
    };

    // Método para realizar a devolução do livro
    void devolverLivro() {
        dataDevolucao <- getCurrentDate();
        livro.disponivel <- true;
    };
};

class Biblioteca {
    list<Livro> livros;
    list<Usuario> usuarios;
    list<Emprestimo> emprestimos;

    // Método para adicionar um novo livro à biblioteca
    void adicionarLivro(titulo: string, anoPublicacao: int) {
        Livro novoLivro <- new Livro(titulo, anoPublicacao);
        livros.append(novoLivro);
    };

    // Método para remover um livro da biblioteca
    void removerLivro(livro: Livro) {
        livros.remove(livro);
    };

    // Método para adicionar um novo usuário à biblioteca
    void adicionarUsuario(nome: string, idade: int) {
        Usuario novoUsuario <- new Usuario(nome, idade);
        usuarios.append(novoUsuario);
    };

    // Método para remover um usuário da biblioteca
    void removerUsuario(usuario: Usuario) {
        usuarios.remove(usuario);
    };

    // Método para realizar o empréstimo de um livro
    void realizarEmprestimo(livro: Livro, usuario: Usuario) {
        if (livro.estaDisponivel()) {
            Emprestimo novoEmprestimo <- new Emprestimo(livro, usuario);
            emprestimos.append(novoEmprestimo);
            livro.disponivel <- false;
        } else {
            print("Livro indisponível para empréstimo");
        }
    };

    // Método para verificar os livros disponíveis na biblioteca
    list<Livro> livrosDisponiveis(): list<Livro> {
        list<Livro> disponiveis;
        foreach (livro in livros) {
            if (livro.estaDisponivel()) {
                disponiveis.append(livro);
            }
        }
        return disponiveis;
    };
};

// Exemplo de uso do sistema de gerenciamento de biblioteca
Biblioteca minhaBiblioteca <- new Biblioteca();

// Adicionar livros
minhaBiblioteca.adicionarLivro("Dom Casmurro", 1899);
minhaBiblioteca.adicionarLivro("A Metamorfose", 1915);
minhaBiblioteca.adicionarLivro("1984", 1949);

// Adicionar usuários
minhaBiblioteca.adicionarUsuario("João", 25);
minhaBiblioteca.adicionarUsuario("Maria", 30);

// Realizar empréstimos
minhaBiblioteca.realizarEmprestimo(minhaBiblioteca.livros[0], minhaBiblioteca.usuarios[0]);
minhaBiblioteca.realizarEmprestimo(minhaBiblioteca.livros[1], minhaBiblioteca.usuarios[1]);

// Realizar devolução
minhaBiblioteca.emprestimos[0].devolverLivro();

// Verificar livros disponíveis
list<Livro> disponiveis <- minhaBiblioteca.livrosDisponiveis();

// Exibir livros disponíveis
foreach (livro in disponiveis) {
    print(livro.titulo);
}
```

Este código representa um sistema básico de gerenciamento de biblioteca em COOL. Ele possui classes para Livro, Usuário e Empréstimo, com métodos para adicionar, remover e consultar objetos dessas classes. Além disso, é possível realizar empréstimos e devoluções de livros, bem como verificar quais livros estão disponíveis na biblioteca. O código também inclui um exemplo de uso do sistema, adicionando livros, usuários, realizando empréstimos, devoluções e exibindo livros disponíveis.

Espero que isso atenda às suas expectativas!