Claro! Aqui está um código complexo em TypeScript que utiliza classes e herança para criar uma aplicação de gerenciamento de funcionários em uma empresa.

```typescript
// Definição da classe base Funcionário
class Funcionario {
  protected nome: string;
  protected endereco: string;
  protected salario: number;

  constructor(nome: string, endereco: string, salario: number) {
    this.nome = nome;
    this.endereco = endereco;
    this.salario = salario;
  }

  // Método para calcular o salário líquido do funcionário
  calcularSalarioLiquido(): number {
    const descontoINSS = this.salario * 0.11; // 11% de desconto para o INSS
    const descontoIRPF = this.salario * 0.275; // 27.5% de desconto para o IRPF

    const salarioLiquido = this.salario - descontoINSS - descontoIRPF;

    return salarioLiquido;
  }

  // Método para exibir os dados do funcionário
  exibirDados(): void {
    console.log(`Nome: ${this.nome}`);
    console.log(`Endereço: ${this.endereco}`);
    console.log(`Salário: R$${this.salario.toFixed(2)}`);
    console.log(`Salário Líquido: R$${this.calcularSalarioLiquido().toFixed(2)}`);
  }
}

// Definição da classe Gerente, que herda da classe Funcionário
class Gerente extends Funcionario {
  private departamento: string;

  constructor(nome: string, endereco: string, salario: number, departamento: string) {
    super(nome, endereco, salario);
    this.departamento = departamento;
  }

  // Método para exibir os dados do gerente
  exibirDados(): void {
    super.exibirDados();
    console.log(`Departamento: ${this.departamento}`);
  }
}

// Definição da classe Desenvolvedor, que herda da classe Funcionário
class Desenvolvedor extends Funcionario {
  private linguagem: string;

  constructor(nome: string, endereco: string, salario: number, linguagem: string) {
    super(nome, endereco, salario);
    this.linguagem = linguagem;
  }

  // Método para exibir os dados do desenvolvedor
  exibirDados(): void {
    super.exibirDados();
    console.log(`Linguagem: ${this.linguagem}`);
  }
}

// Criação de instâncias de funcionários, gerente e desenvolvedor
const func1 = new Funcionario("João", "Rua A, 123", 3000);
const gerente1 = new Gerente("Maria", "Rua B, 456", 5000, "TI");
const dev1 = new Desenvolvedor("Pedro", "Rua C, 789", 4000, "JavaScript");

// Exibição dos dados dos funcionários
func1.exibirDados();
console.log("--------------------");
gerente1.exibirDados();
console.log("--------------------");
dev1.exibirDados();
```

Neste código, temos as classes `Funcionario`, `Gerente` e `Desenvolvedor`. A classe `Funcionario` é a classe base, que contém os atributos e métodos comuns a todos os funcionários, como nome, endereço, salário, método para calcular o salário líquido e método para exibir os dados.

As classes `Gerente` e `Desenvolvedor` herdam da classe `Funcionario` utilizando a palavra-chave `extends`. Elas adicionam atributos específicos para cada tipo de funcionário e sobrescrevem o método `exibirDados()` para exibir os dados adicionais.

No final do código, são criadas instâncias de funcionários, gerente e desenvolvedor, e seus dados são exibidos utilizando o método `exibirDados()`.