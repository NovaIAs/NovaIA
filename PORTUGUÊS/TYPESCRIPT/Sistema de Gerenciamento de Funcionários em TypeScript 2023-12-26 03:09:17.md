Claro! Aqui está um código complexo em TypeScript que utiliza os conceitos de programação orientada a objetos e manipulação de arrays para criar um sistema de gerenciamento de funcionários:

```typescript
class Funcionario {
  private nome: string;
  private cargo: string;
  private salario: number;

  constructor(nome: string, cargo: string, salario: number) {
    this.nome = nome;
    this.cargo = cargo;
    this.salario = salario;
  }

  getNome(): string {
    return this.nome;
  }

  getCargo(): string {
    return this.cargo;
  }

  getSalario(): number {
    return this.salario;
  }

  setSalario(salario: number): void {
    this.salario = salario;
  }

  toString(): string {
    return `Nome: ${this.nome}, Cargo: ${this.cargo}, Salário: ${this.salario}`;
  }
}

class GerenciadorFuncionarios {
  private funcionarios: Funcionario[];

  constructor() {
    this.funcionarios = [];
  }

  adicionarFuncionario(funcionario: Funcionario): void {
    this.funcionarios.push(funcionario);
  }

  removerFuncionario(nome: string): void {
    this.funcionarios = this.funcionarios.filter(
      (funcionario) => funcionario.getNome() !== nome
    );
  }

  listarFuncionarios(): void {
    this.funcionarios.forEach((funcionario) => console.log(funcionario.toString()));
  }

  aumentarSalarioPorCargo(cargo: string, aumento: number): void {
    this.funcionarios.forEach((funcionario) => {
      if (funcionario.getCargo() === cargo) {
        const novoSalario = funcionario.getSalario() * (1 + aumento);
        funcionario.setSalario(novoSalario);
      }
    });
  }
}

// Exemplo de uso

const gerenciador = new GerenciadorFuncionarios();

const funcionario1 = new Funcionario("João", "Desenvolvedor", 5000);
const funcionario2 = new Funcionario("Maria", "Analista", 6000);
const funcionario3 = new Funcionario("Pedro", "Gerente", 8000);

gerenciador.adicionarFuncionario(funcionario1);
gerenciador.adicionarFuncionario(funcionario2);
gerenciador.adicionarFuncionario(funcionario3);

gerenciador.listarFuncionarios(); // Nome: João, Cargo: Desenvolvedor, Salário: 5000
                                // Nome: Maria, Cargo: Analista, Salário: 6000
                                // Nome: Pedro, Cargo: Gerente, Salário: 8000

gerenciador.removerFuncionario("Maria");

gerenciador.listarFuncionarios(); // Nome: João, Cargo: Desenvolvedor, Salário: 5000
                                // Nome: Pedro, Cargo: Gerente, Salário: 8000

gerenciador.aumentarSalarioPorCargo("Desenvolvedor", 0.1);

gerenciador.listarFuncionarios(); // Nome: João, Cargo: Desenvolvedor, Salário: 5500
                                // Nome: Pedro, Cargo: Gerente, Salário: 8000
```

Neste código, criamos duas classes: `Funcionario` e `GerenciadorFuncionarios`. A classe `Funcionario` representa um funcionário com nome, cargo e salário. Ela possui métodos para acessar e modificar esses atributos, bem como um método `toString()` para exibir as informações do funcionário.

A classe `GerenciadorFuncionarios` é responsável por gerenciar uma lista de funcionários. Ela possui métodos para adicionar e remover funcionários, listar todos os funcionários e aumentar o salário de todos os funcionários de um determinado cargo.

No exemplo de uso, criamos três funcionários, adicionamos ao gerenciador, listamos os funcionários, removemos um funcionário, listamos novamente e, por fim, aumentamos o salário de todos os desenvolvedores em 10%.

Esse código é apenas um exemplo de uma implementação complexa em TypeScript. É importante lembrar que a complexidade e tamanho do código podem variar dependendo dos requisitos específicos de cada projeto.