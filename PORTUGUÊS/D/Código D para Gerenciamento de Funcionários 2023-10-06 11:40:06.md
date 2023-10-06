Claro! Aqui está um código complexo na linguagem D para criar um sistema de gerenciamento de funcionários:

```d
import std.stdio;

struct Funcionario {
    int id;
    string nome;
    string cargo;
    float salario;
}

void adicionarFuncionario(ref Funcionario[] funcionarios) {
    Funcionario novoFuncionario;

    writeln("Digite o ID do funcionário:");
    readf("%d", &novoFuncionario.id);

    writeln("Digite o nome do funcionário:");
    readf("%s", &novoFuncionario.nome);

    writeln("Digite o cargo do funcionário:");
    readf("%s", &novoFuncionario.cargo);

    writeln("Digite o salário do funcionário:");
    readf("%f", &novoFuncionario.salario);

    funcionarios ~= novoFuncionario;
    writeln("Funcionário adicionado com sucesso!");
}

void listarFuncionarios(Funcionario[] funcionarios) {
    writeln("Lista de funcionários:");

    foreach (funcionario; funcionarios) {
        writeln("ID:", funcionario.id);
        writeln("Nome:", funcionario.nome);
        writeln("Cargo:", funcionario.cargo);
        writeln("Salário:", funcionario.salario);
        writeln();
    }
}

void buscarFuncionario(Funcionario[] funcionarios) {
    int id;

    writeln("Digite o ID do funcionário que deseja buscar:");
    readf("%d", &id);

    foreach (funcionario; funcionarios) {
        if (funcionario.id == id) {
            writeln("Funcionário encontrado:");
            writeln("ID:", funcionario.id);
            writeln("Nome:", funcionario.nome);
            writeln("Cargo:", funcionario.cargo);
            writeln("Salário:", funcionario.salario);
            return;
        }
    }

    writeln("Funcionário não encontrado!");
}

void menu() {
    Funcionario[] funcionarios;

    while (true) {
        writeln("=== Sistema de Gerenciamento de Funcionários ===");
        writeln("1 - Adicionar funcionário");
        writeln("2 - Listar funcionários");
        writeln("3 - Buscar funcionário");
        writeln("0 - Sair");
        writeln("==============================================");

        int opcao;
        readf("%d", &opcao);

        switch (opcao) {
            case 1:
                adicionarFuncionario(funcionarios);
                break;
            case 2:
                listarFuncionarios(funcionarios);
                break;
            case 3:
                buscarFuncionario(funcionarios);
                break;
            case 0:
                return;
            default:
                writeln("Opção inválida!");
        }

        writeln();
    }
}

void main() {
    menu();
}
```

Este código implementa um sistema de gerenciamento de funcionários em D. Ele possui uma estrutura `Funcionario` que contém informações como ID, nome, cargo e salário. O programa permite adicionar funcionários, listar todos os funcionários cadastrados e buscar um funcionário pelo seu ID.

A função `adicionarFuncionario` solicita ao usuário que insira as informações do novo funcionário e adiciona-o ao array `funcionarios`.

A função `listarFuncionarios` percorre o array `funcionarios` e exibe as informações de cada funcionário.

A função `buscarFuncionario` solicita ao usuário que insira o ID do funcionário que deseja buscar e verifica se há algum funcionário com esse ID no array `funcionarios`. Em caso positivo, exibe as informações do funcionário encontrado.

A função `menu` é responsável por exibir um menu com opções para o usuário interagir com o sistema. Ela utiliza um loop infinito para permitir que o usuário realize várias operações consecutivas até optar por sair.

A função `main` chama a função `menu` para iniciar o programa.

Espero que este código possa lhe ajudar!