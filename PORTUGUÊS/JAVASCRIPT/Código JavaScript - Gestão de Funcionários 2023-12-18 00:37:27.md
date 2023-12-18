Claro! Aqui está um código complexo em JavaScript, que realiza uma série de tarefas diferentes. Vou explicar cada parte do código para facilitar o entendimento:

```javascript
// Declaração de variáveis
let listaNomes = ['João', 'Maria', 'José', 'Ana', 'Pedro'];
let listaIdades = [25, 30, 42, 19, 35];
let listaSalarios = [2500, 4000, 3500, 2800, 5000];
let listaFuncionarios = [];

// Classe Funcionario
class Funcionario {
  constructor(nome, idade, salario) {
    this.nome = nome;
    this.idade = idade;
    this.salario = salario;
  }

  aumentarSalario(percentual) {
    this.salario += (this.salario * percentual) / 100;
  }
}

// Função para criar objetos Funcionario e adicioná-los à listaFuncionarios
function criarFuncionarios() {
  for (let i = 0; i < listaNomes.length; i++) {
    let funcionario = new Funcionario(
      listaNomes[i],
      listaIdades[i],
      listaSalarios[i]
    );
    listaFuncionarios.push(funcionario);
  }
}

// Função para calcular a média de salários dos funcionários
function calcularMediaSalarios() {
  let somaSalarios = 0;
  for (let funcionario of listaFuncionarios) {
    somaSalarios += funcionario.salario;
  }
  let mediaSalarios = somaSalarios / listaFuncionarios.length;
  return mediaSalarios;
}

// Função para encontrar o funcionário mais novo
function encontrarFuncionarioMaisNovo() {
  let funcionarioMaisNovo = listaFuncionarios[0];
  for (let funcionario of listaFuncionarios) {
    if (funcionario.idade < funcionarioMaisNovo.idade) {
      funcionarioMaisNovo = funcionario;
    }
  }
  return funcionarioMaisNovo;
}

// Chamada das funções e exibição dos resultados
criarFuncionarios();
let mediaSalarios = calcularMediaSalarios();
let funcionarioMaisNovo = encontrarFuncionarioMaisNovo();

console.log('Lista de Funcionários:', listaFuncionarios);
console.log('Média de Salários:', mediaSalarios);
console.log('Funcionário Mais Novo:', funcionarioMaisNovo);
```

Neste código, começamos declarando as variáveis `listaNomes`, `listaIdades` e `listaSalarios`, que contêm informações fictícias sobre os funcionários. Em seguida, temos a declaração da lista `listaFuncionarios`, que será preenchida posteriormente.

Em seguida, temos a declaração da classe `Funcionario`, que possui um construtor para definir as propriedades `nome`, `idade` e `salario`, e um método `aumentarSalario` para incrementar o salário do funcionário com base em um percentual fornecido.

A função `criarFuncionarios` é responsável por iterar sobre as listas `listaNomes`, `listaIdades` e `listaSalarios`, criar objetos `Funcionario` com os valores correspondentes e adicioná-los à lista `listaFuncionarios`.

A função `calcularMediaSalarios` percorre a lista de funcionários e calcula a média dos salários, retornando esse valor.

A função `encontrarFuncionarioMaisNovo` percorre a lista de funcionários e encontra o funcionário mais novo com base na idade.

Por fim, chamamos as funções `criarFuncionarios`, `calcularMediaSalarios` e `encontrarFuncionarioMaisNovo`, armazenamos os resultados em variáveis e exibimos os dados no console.

Espero que este código atenda às suas expectativas!