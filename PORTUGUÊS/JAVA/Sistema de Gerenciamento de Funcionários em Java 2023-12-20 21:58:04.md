Claro! Aqui está um código complexo em Java que envolve a criação de um sistema de gerenciamento de funcionários para uma empresa fictícia:

```java
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

class Funcionario {
    private int id;
    private String nome;
    private String cargo;
    private double salario;

    public Funcionario(int id, String nome, String cargo, double salario) {
        this.id = id;
        this.nome = nome;
        this.cargo = cargo;
        this.salario = salario;
    }

    public int getId() {
        return id;
    }

    public String getNome() {
        return nome;
    }

    public String getCargo() {
        return cargo;
    }

    public double getSalario() {
        return salario;
    }

    public void setSalario(double salario) {
        this.salario = salario;
    }

    @Override
    public String toString() {
        return "ID: " + id + ", Nome: " + nome + ", Cargo: " + cargo + ", Salário: " + salario;
    }
}

class GerenciadorFuncionarios {
    private List<Funcionario> funcionarios;
    private int proximoId;

    public GerenciadorFuncionarios() {
        this.funcionarios = new ArrayList<>();
        this.proximoId = 1;
    }

    public void adicionarFuncionario(String nome, String cargo, double salario) {
        Funcionario funcionario = new Funcionario(proximoId, nome, cargo, salario);
        funcionarios.add(funcionario);
        proximoId++;
        System.out.println("Funcionário adicionado com sucesso!");
    }

    public void removerFuncionario(int id) {
        boolean encontrado = false;
        for (Funcionario funcionario : funcionarios) {
            if (funcionario.getId() == id) {
                funcionarios.remove(funcionario);
                encontrado = true;
                System.out.println("Funcionário removido com sucesso!");
                break;
            }
        }
        if (!encontrado) {
            System.out.println("Funcionário não encontrado!");
        }
    }

    public void aumentarSalario(int id, double aumento) {
        boolean encontrado = false;
        for (Funcionario funcionario : funcionarios) {
            if (funcionario.getId() == id) {
                double novoSalario = funcionario.getSalario() * (1 + aumento);
                funcionario.setSalario(novoSalario);
                encontrado = true;
                System.out.println("Salário do funcionário atualizado!");
                break;
            }
        }
        if (!encontrado) {
            System.out.println("Funcionário não encontrado!");
        }
    }

    public void imprimirFuncionarios() {
        for (Funcionario funcionario : funcionarios) {
            System.out.println(funcionario);
        }
    }
}

public class SistemaGerenciamentoFuncionarios {
    public static void main(String[] args) {
        GerenciadorFuncionarios gerenciador = new GerenciadorFuncionarios();
        Scanner scanner = new Scanner(System.in);

        int opcao = 0;
        while (opcao != 5) {
            System.out.println("----- Sistema de Gerenciamento de Funcionários -----");
            System.out.println("1 - Adicionar Funcionário");
            System.out.println("2 - Remover Funcionário");
            System.out.println("3 - Aumentar Salário");
            System.out.println("4 - Imprimir Funcionários");
            System.out.println("5 - Sair");
            System.out.println("---------------------------------------------------");
            System.out.print("Selecione uma opção: ");
            opcao = scanner.nextInt();

            switch (opcao) {
                case 1:
                    System.out.print("Digite o nome do funcionário: ");
                    scanner.nextLine();
                    String nome = scanner.nextLine();
                    System.out.print("Digite o cargo do funcionário: ");
                    String cargo = scanner.nextLine();
                    System.out.print("Digite o salário do funcionário: ");
                    double salario = scanner.nextDouble();
                    gerenciador.adicionarFuncionario(nome, cargo, salario);
                    break;
                case 2:
                    System.out.print("Digite o ID do funcionário a ser removido: ");
                    int idRemover = scanner.nextInt();
                    gerenciador.removerFuncionario(idRemover);
                    break;
                case 3:
                    System.out.print("Digite o ID do funcionário: ");
                    int idAumentarSalario = scanner.nextInt();
                    System.out.print("Digite o aumento (em decimal): ");
                    double aumento = scanner.nextDouble();
                    gerenciador.aumentarSalario(idAumentarSalario, aumento);
                    break;
                case 4:
                    gerenciador.imprimirFuncionarios();
                    break;
                case 5:
                    System.out.println("Saindo do sistema...");
                    break;
                default:
                    System.out.println("Opção inválida! Tente novamente.");
            }

            System.out.println();
        }

        scanner.close();
    }
}
```

Este código implementa um sistema de gerenciamento de funcionários que permite adicionar, remover, aumentar salário e imprimir os funcionários cadastrados. 

A classe `Funcionario` é usada para representar cada funcionário e contém os atributos `id`, `nome`, `cargo` e `salario`. A classe `GerenciadorFuncionarios` é responsável por gerenciar a lista de funcionários e implementa os métodos para adicionar, remover, aumentar salário e imprimir os funcionários.

A classe `SistemaGerenciamentoFuncionarios` é usada para executar o programa, interagindo com o usuário através de um menu de opções. É criada uma instância do `GerenciadorFuncionarios` e um objeto `Scanner` para ler as entradas do usuário.

O programa fica em um loop enquanto a opção selecionada pelo usuário for diferente de 5 (sair). Em cada iteração, o menu é exibido e a opção escolhida é processada através de um switch case.

Espero que este código atenda às suas expectativas! Se precisar de mais alguma coisa, estou à disposição.