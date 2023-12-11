Claro! Aqui está um exemplo de código complexo em TypeScript:

```typescript
// Classe Pessoa representa uma pessoa com nome e idade
class Pessoa {
  nome: string;
  idade: number;
  
  constructor(nome: string, idade: number) {
    this.nome = nome;
    this.idade = idade;
  }
  
  // Método para imprimir os detalhes da pessoa
  imprimirDetalhes(): void {
    console.log(`Nome: ${this.nome}`);
    console.log(`Idade: ${this.idade}`);
  }
}

// Classe Banco representa um banco com várias contas
class Banco {
  contas: Conta[];
  
  constructor() {
    this.contas = [];
  }
  
  // Método para adicionar uma conta ao banco
  adicionarConta(conta: Conta): void {
    this.contas.push(conta);
  }
  
  // Método para imprimir as informações de todas as contas do banco
  imprimirInformacoesContas(): void {
    this.contas.forEach((conta) => {
      console.log(`Número da Conta: ${conta.numeroConta}`);
      console.log(`Saldo: R$ ${conta.saldo}`);
      console.log(`-----------------------------`);
    });
  }
}

// Classe Conta representa uma conta bancária com número da conta e saldo
class Conta {
  numeroConta: string;
  saldo: number;
  
  constructor(numeroConta: string, saldo: number) {
    this.numeroConta = numeroConta;
    this.saldo = saldo;
  }
  
  // Método para depositar um valor na conta
  depositar(valor: number): void {
    this.saldo += valor;
  }
  
  // Método para sacar um valor da conta
  sacar(valor: number): void {
    if (valor <= this.saldo) {
      this.saldo -= valor;
    } else {
      console.log(`Saldo insuficiente para sacar o valor especificado.`);
    }
  }
}

// Criando instâncias das classes Pessoa, Banco e Conta
const pessoa1 = new Pessoa("João", 25);
const pessoa2 = new Pessoa("Maria", 30);

const banco = new Banco();

const conta1 = new Conta("123456", 1000);
const conta2 = new Conta("654321", 2000);

// Adicionando contas ao banco
banco.adicionarConta(conta1);
banco.adicionarConta(conta2);

// Imprimindo detalhes das pessoas
pessoa1.imprimirDetalhes();
console.log(`-----------------------------`);
pessoa2.imprimirDetalhes();

// Imprimindo informações das contas do banco
console.log(`-----------------------------`);
banco.imprimirInformacoesContas();

// Realizando operações bancárias
console.log(`-----------------------------`);
conta1.depositar(500);
conta2.sacar(300);

// Imprimindo informações atualizadas das contas do banco
console.log(`-----------------------------`);
banco.imprimirInformacoesContas();
```

Este código é um exemplo de um sistema bancário simples, onde temos as classes `Pessoa`, `Banco` e `Conta`. A classe `Pessoa` representa uma pessoa com nome e idade, a classe `Banco` representa um banco com várias contas e a classe `Conta` representa uma conta bancária com número da conta e saldo.

No código, criamos duas instâncias da classe `Pessoa` (`pessoa1` e `pessoa2`) e duas instâncias da classe `Conta` (`conta1` e `conta2`). Em seguida, adicionamos as contas ao banco utilizando o método `adicionarConta` da classe `Banco`. Depois, imprimimos os detalhes das pessoas e as informações das contas do banco.

Em seguida, realizamos algumas operações bancárias, como depositar e sacar valores das contas. E finalmente, imprimimos as informações atualizadas das contas do banco.

Este código é apenas um exemplo de um sistema bancário básico em TypeScript. É importante ressaltar que o código pode ser expandido e aprimorado de acordo com as necessidades específicas do projeto.