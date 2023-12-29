Claro! Aqui está um código complexo em D que envolve a criação de uma agenda de contatos com funcionalidades avançadas:

```d
import std.stdio;

struct Contato {
    string nome;
    string telefone;
    string email;
}

class Agenda {
    private Contato[] contatos;

    this() {
        contatos = new Contato[](0);
    }

    void adicionarContato(Contato contato) {
        contatos ~= contato;
    }

    void removerContato(Contato contato) {
        foreach (i, c; contatos) {
            if (contato.nome == c.nome && contato.telefone == c.telefone && contato.email == c.email) {
                contatos = contatos.remove(i);
                break;
            }
        }
    }

    void buscarContato(string nome) {
        bool encontrado = false;
        foreach (c; contatos) {
            if (c.nome == nome) {
                encontrado = true;
                writeln("Nome: ", c.nome);
                writeln("Telefone: ", c.telefone);
                writeln("Email: ", c.email);
                writeln();
            }
        }

        if (!encontrado) {
            writeln("Contato não encontrado.");
        }
    }

    void listarContatos() {
        foreach (c; contatos) {
            writeln("Nome: ", c.nome);
            writeln("Telefone: ", c.telefone);
            writeln("Email: ", c.email);
            writeln();
        }

        if (contatos.length == 0) {
            writeln("Agenda vazia.");
        }
    }
}

void main() {
    Agenda agenda = new Agenda();

    while (true) {
        writeln("===== MENU =====");
        writeln("1. Adicionar Contato");
        writeln("2. Remover Contato");
        writeln("3. Buscar Contato");
        writeln("4. Listar Contatos");
        writeln("5. Sair");
        writeln("================");
        writeln();

        int opcao;
        write("Digite a opção desejada: ");
        readf("%d", &opcao);
        writeln();

        if (opcao == 1) {
            Contato contato;
            write("Nome: ");
            readln(contato.nome);
            write("Telefone: ");
            readln(contato.telefone);
            write("Email: ");
            readln(contato.email);
            writeln();

            agenda.adicionarContato(contato);
            writeln("Contato adicionado com sucesso!");
        } else if (opcao == 2) {
            Contato contato;
            write("Nome: ");
            readln(contato.nome);
            write("Telefone: ");
            readln(contato.telefone);
            write("Email: ");
            readln(contato.email);
            writeln();

            agenda.removerContato(contato);
            writeln("Contato removido com sucesso!");
        } else if (opcao == 3) {
            string nome;
            write("Nome: ");
            readln(nome);
            writeln();

            agenda.buscarContato(nome);
        } else if (opcao == 4) {
            agenda.listarContatos();
        } else if (opcao == 5) {
            break;
        } else {
            writeln("Opção inválida.");
        }

        writeln();
    }
}
```

Neste código, temos a definição de uma struct `Contato`, que representa um contato com nome, telefone e email. Em seguida, temos a classe `Agenda`, que possui um array de contatos e métodos para adicionar, remover, buscar e listar contatos.

No `main()`, temos um loop que exibe um menu com opções para o usuário interagir com a agenda. O usuário pode escolher adicionar um contato, remover um contato, buscar um contato, listar todos os contatos ou sair do programa. Cada opção é tratada de forma adequada, interagindo com a agenda e exibindo mensagens informativas para o usuário.

Este código é um exemplo de como você pode implementar uma agenda de contatos com funcionalidades básicas em D.