Claro! Aqui está um exemplo de código complexo em JavaScript que realiza um sistema de gerenciamento de funcionários em uma empresa:

```javascript
// Criação da classe Funcionario
class Funcionario {
  constructor(nome, cargo, salario) {
    this.nome = nome;
    this.cargo = cargo;
    this.salario = salario;
  }

  // Método para aumentar o salário em uma porcentagem específica
  aumentarSalario(porcentagem) {
    this.salario += (this.salario * porcentagem) / 100;
  }

  // Método para exibir os dados do funcionário
  exibirDados() {
    console.log(`Nome: ${this.nome}`);
    console.log(`Cargo: ${this.cargo}`);
    console.log(`Salário: R$ ${this.salario.toFixed(2)}\n`);
  }
}

// Função para adicionar novos funcionários ao sistema
function adicionarFuncionario() {
  let nome = prompt("Digite o nome do funcionário:");
  let cargo = prompt("Digite o cargo do funcionário:");
  let salario = parseFloat(prompt("Digite o salário do funcionário:"));

  // Criação de um novo objeto da classe Funcionario
  let novoFuncionario = new Funcionario(nome, cargo, salario);

  // Adiciona o novo funcionário ao array de funcionários
  funcionarios.push(novoFuncionario);

  console.log("Funcionário adicionado com sucesso!\n");
}

// Função para aumentar o salário de um funcionário existente
function aumentarSalarioFuncionario() {
  let nome = prompt("Digite o nome do funcionário:");

  // Procura o funcionário pelo nome no array de funcionários
  let funcionario = funcionarios.find((f) => f.nome === nome);

  if (funcionario) {
    let porcentagem = parseFloat(
      prompt("Digite a porcentagem de aumento do salário:")
    );

    // Chama o método aumentarSalario() do objeto funcionário encontrado
    funcionario.aumentarSalario(porcentagem);
    console.log("Salário do funcionário atualizado com sucesso!\n");
  } else {
    console.log("Funcionário não encontrado!\n");
  }
}

// Função para exibir os dados de todos os funcionários
function exibirDadosFuncionarios() {
  if (funcionarios.length > 0) {
    console.log("Dados dos funcionários:\n");

    // Percorre o array de funcionários e chama o método exibirDados() de cada objeto
    for (let funcionario of funcionarios) {
      funcionario.exibirDados();
    }
  } else {
    console.log("Nenhum funcionário cadastrado!\n");
  }
}

// Array para armazenar os funcionários
let funcionarios = [];

// Loop principal do sistema
while (true) {
  console.log("=== Sistema de Gerenciamento de Funcionários ===");
  console.log("1. Adicionar funcionário");
  console.log("2. Aumentar salário de um funcionário");
  console.log("3. Exibir dados dos funcionários");
  console.log("4. Sair\n");

  let opcao = parseInt(prompt("Digite uma opção:"));

  switch (opcao) {
    case 1:
      adicionarFuncionario();
      break;
    case 2:
      aumentarSalarioFuncionario();
      break;
    case 3:
      exibirDadosFuncionarios();
      break;
    case 4:
      console.log("Saindo do sistema...");
      return;
    default:
      console.log("Opção inválida!\n");
      break;
  }
}
```

Este código cria uma classe chamada `Funcionario` que possui os atributos `nome`, `cargo` e `salario`. A classe também possui os métodos `aumentarSalario()` para aumentar o salário do funcionário em uma porcentagem informada e `exibirDados()` para exibir os dados do funcionário.

Além disso, o código contém funções para adicionar um novo funcionário, aumentar o salário de um funcionário existente e exibir os dados de todos os funcionários. O sistema principal é um loop que exibe um menu de opções e executa a ação correspondente de acordo com a opção selecionada pelo usuário.

Espero que este exemplo atenda às suas expectativas!