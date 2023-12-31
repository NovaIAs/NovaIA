Certamente! Vou criar um código em JavaScript complexo que envolve manipulação de listas e objetos. Este código tem como objetivo simular um sistema de gerenciamento de funcionários em uma empresa. Vamos lá:

```javascript
// Definição da classe Funcionario
class Funcionario {
  constructor(nome, cargo, salario) {
    this.nome = nome;
    this.cargo = cargo;
    this.salario = salario;
  }

  // Método para aumentar o salário do funcionário
  aumentarSalario(percentual) {
    this.salario += this.salario * (percentual / 100);
  }
}

// Criando uma lista de funcionários
const listaFuncionarios = [
  new Funcionario("João", "Analista de Sistemas", 5000),
  new Funcionario("Maria", "Gerente de Projetos", 8000),
  new Funcionario("Pedro", "Desenvolvedor Front-end", 4000),
];

// Função para buscar funcionário pelo nome
function buscarFuncionario(nome) {
  for (let i = 0; i < listaFuncionarios.length; i++) {
    if (listaFuncionarios[i].nome === nome) {
      return listaFuncionarios[i];
    }
  }
  return null; // Retorna null caso não encontre o funcionário
}

// Função para imprimir todos os funcionários
function imprimirFuncionarios() {
  listaFuncionarios.forEach((funcionario) => {
    console.log(`Nome: ${funcionario.nome}`);
    console.log(`Cargo: ${funcionario.cargo}`);
    console.log(`Salário: R$ ${funcionario.salario.toFixed(2)}`);
    console.log("---------------------");
  });
}

// Exemplo de uso das funções e métodos criados
console.log("Lista de funcionários:");
console.log("---------------------");
imprimirFuncionarios();

console.log("Aumentando o salário do funcionário João em 10%...");
const funcionarioJoao = buscarFuncionario("João");
if (funcionarioJoao) {
  funcionarioJoao.aumentarSalario(10);
} else {
  console.log("Funcionário não encontrado!");
}

console.log("Lista de funcionários atualizada:");
console.log("---------------------");
imprimirFuncionarios();
```

Neste código, criamos uma classe `Funcionario` que possui propriedades como `nome`, `cargo` e `salario`. Também temos um método `aumentarSalario` que aumenta o salário do funcionário com base em um percentual informado.

Em seguida, criamos uma lista de funcionários utilizando a classe `Funcionario`. A função `buscarFuncionario` permite pesquisar um funcionário pelo nome na lista. A função `imprimirFuncionarios` imprime todos os funcionários da lista.

Por fim, fazemos um exemplo de uso das funções e métodos criados, imprimindo a lista de funcionários inicialmente, aumentando o salário do funcionário "João" em 10% e, em seguida, imprimindo a lista de funcionários atualizada.

Espero que esse código atenda às suas expectativas! Se você tiver mais alguma solicitação ou dúvida, fique à vontade para perguntar.