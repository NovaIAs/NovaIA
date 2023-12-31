Claro! Aqui está um exemplo de um código complexo em Java que utiliza conceitos de herança, polimorfismo e interfaces para criar um sistema de gerenciamento de funcionários de uma empresa.

```java
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

// Classe abstrata que representa um funcionário
abstract class Funcionario {
    private String nome;
    private double salario;

    public Funcionario(String nome, double salario) {
        this.nome = nome;
        this.salario = salario;
    }

    public String getNome() {
        return nome;
    }

    public double getSalario() {
        return salario;
    }

    public abstract double calcularBonificacao();

    public abstract void exibirInformacoes();
}

// Classe que representa um funcionário efetivo
class FuncionarioEfetivo extends Funcionario {
    private int horasTrabalhadas;
    private double valorHora;

    public FuncionarioEfetivo(String nome, double salario, int horasTrabalhadas, double valorHora) {
        super(nome, salario);
        this.horasTrabalhadas = horasTrabalhadas;
        this.valorHora = valorHora;
    }

    @Override
    public double calcularBonificacao() {
        return getSalario() * 0.1;
    }

    @Override
    public void exibirInformacoes() {
        System.out.println("Nome: " + getNome());
        System.out.println("Tipo: Funcionário Efetivo");
        System.out.println("Salário: R$" + getSalario());
        System.out.println("Horas Trabalhadas: " + horasTrabalhadas);
        System.out.println("Valor Hora: R$" + valorHora);
        System.out.println("Bonificação: R$" + calcularBonificacao());
    }
}

// Classe que representa um funcionário terceirizado
class FuncionarioTerceirizado extends Funcionario {
    private String nomeEmpresa;

    public FuncionarioTerceirizado(String nome, double salario, String nomeEmpresa) {
        super(nome, salario);
        this.nomeEmpresa = nomeEmpresa;
    }

    @Override
    public double calcularBonificacao() {
        return getSalario() * 0.05;
    }

    @Override
    public void exibirInformacoes() {
        System.out.println("Nome: " + getNome());
        System.out.println("Tipo: Funcionário Terceirizado");
        System.out.println("Salário: R$" + getSalario());
        System.out.println("Empresa: " + nomeEmpresa);
        System.out.println("Bonificação: R$" + calcularBonificacao());
    }
}

// Interface que define operações de um sistema de gerenciamento de funcionários
interface GerenciadorFuncionarios {
    void adicionarFuncionario(Funcionario funcionario);
    void removerFuncionario(String nome);
    void exibirTodosFuncionarios();
}

// Classe que implementa a interface GerenciadorFuncionarios
class SistemaGerenciadorFuncionarios implements GerenciadorFuncionarios {
    private List<Funcionario> funcionarios;

    public SistemaGerenciadorFuncionarios() {
        this.funcionarios = new ArrayList<>();
    }

    @Override
    public void adicionarFuncionario(Funcionario funcionario) {
        funcionarios.add(funcionario);
    }

    @Override
    public void removerFuncionario(String nome) {
        funcionarios.removeIf(funcionario -> funcionario.getNome().equals(nome));
    }

    @Override
    public void exibirTodosFuncionarios() {
        for (Funcionario funcionario : funcionarios) {
            funcionario.exibirInformacoes();
            System.out.println("------------------------------------");
        }
    }
}

public class Main {
    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        SistemaGerenciadorFuncionarios sistema = new SistemaGerenciadorFuncionarios();

        boolean executando = true;
        while (executando) {
            System.out.println("========== SISTEMA DE GERENCIAMENTO DE FUNCIONÁRIOS ==========");
            System.out.println("1. Adicionar Funcionário Efetivo");
            System.out.println("2. Adicionar Funcionário Terceirizado");
            System.out.println("3. Remover Funcionário");
            System.out.println("4. Exibir Todos os Funcionários");
            System.out.println("5. Sair");
            System.out.print("Digite a opção desejada: ");
            int opcao = scanner.nextInt();
            scanner.nextLine(); // Limpar o buffer do scanner

            switch (opcao) {
                case 1:
                    System.out.print("Digite o nome do funcionário efetivo: ");
                    String nomeEfetivo = scanner.nextLine();
                    System.out.print("Digite o salário do funcionário efetivo: ");
                    double salarioEfetivo = scanner.nextDouble();
                    System.out.print("Digite a quantidade de horas trabalhadas: ");
                    int horasTrabalhadas = scanner.nextInt();
                    System.out.print("Digite o valor da hora: ");
                    double valorHora = scanner.nextDouble();
                    FuncionarioEfetivo efetivo = new FuncionarioEfetivo(nomeEfetivo, salarioEfetivo, horasTrabalhadas, valorHora);
                    sistema.adicionarFuncionario(efetivo);
                    break;
                case 2:
                    System.out.print("Digite o nome do funcionário terceirizado: ");
                    String nomeTerceirizado = scanner.nextLine();
                    System.out.print("Digite o salário do funcionário terceirizado: ");
                    double salarioTerceirizado = scanner.nextDouble();
                    scanner.nextLine(); // Limpar o buffer do scanner
                    System.out.print("Digite o nome da empresa: ");
                    String nomeEmpresa = scanner.nextLine();
                    FuncionarioTerceirizado terceirizado = new FuncionarioTerceirizado(nomeTerceirizado, salarioTerceirizado, nomeEmpresa);
                    sistema.adicionarFuncionario(terceirizado);
                    break;
                case 3:
                    System.out.print("Digite o nome do funcionário que deseja remover: ");
                    String nomeRemover = scanner.nextLine();
                    sistema.removerFuncionario(nomeRemover);
                    break;
                case 4:
                    sistema.exibirTodosFuncionarios();
                    break;
                case 5:
                    executando = false;
                    break;
                default:
                    System.out.println("Opção inválida!");
                    break;
            }
        }

        scanner.close();
    }
}
```

Neste código, temos a definição de três classes: `Funcionario`, `FuncionarioEfetivo` e `FuncionarioTerceirizado`. A classe `Funcionario` é uma classe abstrata que contém atributos comuns a todos os funcionários, como nome e salário, e métodos abstratos para calcular a bonificação e exibir as informações.

As classes `FuncionarioEfetivo` e `FuncionarioTerceirizado` são subclasses da classe `Funcionario` e implementam os métodos abstratos de acordo com suas particularidades. A classe `FuncionarioEfetivo` possui atributos adicionais, como horas trabalhadas e valor da hora, enquanto a classe `FuncionarioTerceirizado` possui o nome da empresa.

A interface `GerenciadorFuncionarios` define as operações básicas de um sistema de gerenciamento de funcionários, como adicionar, remover e exibir funcionários. A classe `SistemaGerenciadorFuncionarios` implementa essa interface e utiliza uma lista para armazenar os funcionários.

No `Main`, temos um exemplo de uso do sistema de gerenciamento de funcionários. O usuário pode adicionar funcionários efetivos, funcionários terceirizados, remover funcionários e exibir todos os funcionários cadastrados.

Espero que este exemplo seja útil para você entender conceitos avançados de programação em Java!