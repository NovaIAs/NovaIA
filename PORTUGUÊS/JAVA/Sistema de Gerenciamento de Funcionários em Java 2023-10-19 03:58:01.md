Claro, vou criar um código em Java que implementa um sistema de gerenciamento de funcionários de uma empresa. O código será dividido em classes para organizar melhor as funcionalidades.

Primeiro, vamos criar a classe "Funcionario" que representa um funcionário da empresa:

```java
public class Funcionario {
    private String nome;
    private double salario;
    
    public Funcionario(String nome, double salario) {
        this.nome = nome;
        this.salario = salario;
    }
    
    public String getNome() {
        return nome;
    }
    
    public void setNome(String nome) {
        this.nome = nome;
    }
    
    public double getSalario() {
        return salario;
    }
    
    public void setSalario(double salario) {
        this.salario = salario;
    }
    
    public void imprimirDados() {
        System.out.println("Nome: " + nome);
        System.out.println("Salário: " + salario);
    }
}
```

A classe "Funcionario" possui os atributos "nome" e "salario", e os métodos de acesso (getters) e modificação (setters) para esses atributos. O método "imprimirDados" imprime na tela o nome e o salário do funcionário.

Agora, vamos criar a classe principal "Empresa" que irá gerenciar os funcionários:

```java
import java.util.ArrayList;
import java.util.List;

public class Empresa {
    private List<Funcionario> funcionarios;
    
    public Empresa() {
        funcionarios = new ArrayList<>();
    }
    
    public void contratarFuncionario(String nome, double salario) {
        Funcionario funcionario = new Funcionario(nome, salario);
        funcionarios.add(funcionario);
        System.out.println("Funcionário contratado com sucesso!");
    }
    
    public void demitirFuncionario(String nome) {
        for (Funcionario funcionario : funcionarios) {
            if (funcionario.getNome().equals(nome)) {
                funcionarios.remove(funcionario);
                System.out.println("Funcionário demitido com sucesso!");
                return;
            }
        }
        System.out.println("Funcionário não encontrado.");
    }
    
    public void imprimirFuncionarios() {
        for (Funcionario funcionario : funcionarios) {
            funcionario.imprimirDados();
            System.out.println("--------------------");
        }
    }
}
```

A classe "Empresa" possui uma lista de objetos da classe "Funcionario" para armazenar os funcionários contratados. O método "contratarFuncionario" recebe o nome e o salário do funcionário, cria um novo objeto "Funcionario" e adiciona-o à lista. O método "demitirFuncionario" procura na lista pelo funcionário com o nome informado e remove-o, caso seja encontrado. O método "imprimirFuncionarios" percorre a lista de funcionários e imprime os dados de cada um.

Aqui está um exemplo de uso do código:

```java
public class Main {
    public static void main(String[] args) {
        Empresa empresa = new Empresa();
        
        empresa.contratarFuncionario("João", 2500.0);
        empresa.contratarFuncionario("Maria", 3000.0);
        empresa.contratarFuncionario("Pedro", 2000.0);
        
        empresa.imprimirFuncionarios();
        
        empresa.demitirFuncionario("Maria");
        
        empresa.imprimirFuncionarios();
    }
}
```

Neste exemplo, a empresa contrata três funcionários, imprime os dados de todos eles e em seguida demite a funcionária "Maria". Por fim, imprime novamente os dados dos funcionários restantes.

Espero que esse código atenda às suas expectativas. Lembre-se de que você pode modificar e expandir o código de acordo com suas necessidades.