Claro! Abaixo está um código em COOL (Classroom Object-Oriented Language) que implementa um sistema de gerenciamento de uma biblioteca virtual. O código possui classes para representar livros, usuários e a biblioteca em si. Além disso, ele permite que os usuários realizem ações como adicionar e remover livros, fazer empréstimos, devoluções, entre outras funcionalidades.

```
class Livro {

    atributo titulo : String
    atributo autor : String
    atributo disponivel : Bool

    inicializacao(t : String, a : String) : SELF_TYPE {
        {
            self.titulo <- t;
            self.autor <- a;
            self.disponivel <- true;
            self
        }
    }

    metodo emprestar() : Object {
        if self.disponivel then
            {
                self.disponivel <- false;
                "Livro emprestado com sucesso!"
            }
        else
            "Livro não disponível para empréstimo!"
    }

    metodo devolver() : Object {
        if not self.disponivel then
            {
                self.disponivel <- true;
                "Livro devolvido com sucesso!"
            }
        else
            "Este livro já está disponível na biblioteca!"
    }
}

class Usuario {

    atributo nome : String
    atributo livrosEmprestados : Set(Livro)

    inicializacao(n : String) : SELF_TYPE {
        {
            self.nome <- n;
            self.livrosEmprestados <- {};
            self
        }
    }

    metodo emprestarLivro(l : Livro) : Object {
        if l.disponivel then
            {
                self.livrosEmprestados.add(l);
                l.emprestar()
            }
        else
            "Livro não disponível para empréstimo!"
    }

    metodo devolverLivro(l : Livro) : Object {
        if self.livrosEmprestados.contains(l) then
            {
                self.livrosEmprestados.remove(l);
                l.devolver()
            }
        else
            "Este livro não foi emprestado por você!"
    }
}

class Biblioteca {

    atributo livros : List(Livro)
    atributo usuarios : List(Usuario)

    inicializacao() : SELF_TYPE {
        {
            self.livros <- new List;
            self.usuarios <- new List;
            self
        }
    }

    metodo adicionarLivro(t : String, a : String) : Object {
        let novoLivro : Livro <- new Livro.inicializacao(t, a);
        self.livros.add(novoLivro);
        "Livro adicionado com sucesso!"
    }

    metodo removerLivro(l : Livro) : Object {
        if self.livros.contains(l) then
            {
                if l.disponivel then
                    {
                        self.livros.remove(l);
                        "Livro removido com sucesso!"
                    }
                else
                    "Este livro está emprestado e não pode ser removido!"
            }
        else
            "Este livro não está cadastrado na biblioteca!"
    }

    metodo adicionarUsuario(n : String) : Object {
        let novoUsuario : Usuario <- new Usuario.inicializacao(n);
        self.usuarios.add(novoUsuario);
        "Usuário adicionado com sucesso!"
    }

    metodo removerUsuario(u : Usuario) : Object {
        if self.usuarios.contains(u) then
            {
                if u.livrosEmprestados.size = 0 then
                    {
                        self.usuarios.remove(u);
                        "Usuário removido com sucesso!"
                    }
                else
                    "Este usuário possui livros emprestados e não pode ser removido!"
            }
        else
            "Este usuário não está cadastrado na biblioteca!"
    }
}

```

Nesse código, a classe `Livro` representa um livro, com atributos como título, autor e disponibilidade. Ela possui métodos como `emprestar()` e `devolver()`, que permitem ao usuário realizar ações de empréstimo e devolução do livro.

A classe `Usuario` representa um usuário da biblioteca, com atributos como nome e uma lista de livros emprestados. Ela possui métodos como `emprestarLivro()` e `devolverLivro()`, que permitem ao usuário realizar ações de empréstimo e devolução de livros.

A classe `Biblioteca` representa a biblioteca em si, com atributos como uma lista de livros e uma lista de usuários. Ela possui métodos como `adicionarLivro()`, `removerLivro()`, `adicionarUsuario()` e `removerUsuario()`, que permitem ao usuário realizar ações como adicionar e remover livros, adicionar e remover usuários, entre outras funcionalidades.

Esse código em COOL é apenas uma implementação básica e pode ser estendido com mais funcionalidades, validações e tratamentos de erros.