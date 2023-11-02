Certamente! Aqui está um exemplo complexo em Java que cria um sistema de gerenciamento de funcionários em uma empresa fictícia. Este código envolve várias classes, encapsulamento de dados, herança, e manipulação de arrays para armazenar e gerenciar informações dos funcionários.

```java
// Classe Funcionario para representar os funcionários
class Funcionario {
    private int id;
    private String nome;
    private double salario;

    public Funcionario(int id, String nome, double salario) {
        this.id = id;
        this.nome = nome;
        this.salario = salario;
    }

    public int getId() {
        return id;
    }

    public String getNome() {
        return nome;
    }

    public double getSalario() {
        return salario;
    }

    public void aumentarSalario(double aumento) {
        this.salario += aumento;
    }

    @Override
    public String toString() {
        return "ID: " + id + ", Nome: " + nome + ", Salário: R$" + salario;
    }
}

// Classe Gerente que herda de Funcionario
class Gerente extends Funcionario {
    private String departamento;

    public Gerente(int id, String nome, double salario, String departamento) {
        super(id, nome, salario);
        this.departamento = departamento;
    }

    public String getDepartamento() {
        return departamento;
    }

    @Override
    public String toString() {
        return "Gerente - " + super.toString() + ", Departamento: " + departamento;
    }
}

// Classe Empresa para gerenciar os funcionários
class Empresa {
    private Funcionario[] funcionarios;
    private int contadorFuncionarios;

    public Empresa(int tamanhoMaximo) {
        funcionarios = new Funcionario[tamanhoMaximo];
        contadorFuncionarios = 0;
    }

    public void adicionarFuncionario(Funcionario funcionario) {
        if (contadorFuncionarios < funcionarios.length) {
            funcionarios[contadorFuncionarios] = funcionario;
            contadorFuncionarios++;
        } else {
            System.out.println("Não é possível adicionar mais funcionários. Limite atingido.");
        }
    }

    public void mostrarFuncionarios() {
        System.out.println("Lista de Funcionários:");
        for (int i = 0; i < contadorFuncionarios; i++) {
            System.out.println(funcionarios[i]);
        }
    }
}

// Exemplo de utilização das classes
public class Main {
    public static void main(String[] args) {
        Empresa minhaEmpresa = new Empresa(10);

        Funcionario func1 = new Funcionario(1, "João", 3000.0);
        Funcionario func2 = new Funcionario(2, "Maria", 3500.0);
        Gerente gerente1 = new Gerente(3, "Carlos", 5000.0, "TI");

        minhaEmpresa.adicionarFuncionario(func1);
        minhaEmpresa.adicionarFuncionario(func2);
        minhaEmpresa.adicionarFuncionario(gerente1);

        minhaEmpresa.mostrarFuncionarios();
    }
}
```

Este código cria um sistema de funcionários onde é possível adicionar funcionários simples e gerentes, exibir suas informações e armazenar os dados em uma estrutura de dados. Ele utiliza conceitos de orientação a objetos como herança, encapsulamento e polimorfismo para gerenciar os funcionários de uma empresa fictícia.