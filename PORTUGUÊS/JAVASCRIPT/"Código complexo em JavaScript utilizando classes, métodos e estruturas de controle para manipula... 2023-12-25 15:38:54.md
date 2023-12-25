Claro! Aqui está um exemplo de um código complexo em JavaScript, escrito em português, que envolve o uso de classes, métodos, manipulação de arrays e estruturas de controle. 

```
// Classe Pessoa
class Pessoa {
  constructor(nome, idade) {
    this.nome = nome;
    this.idade = idade;
  }

  saudacao() {
    console.log(`Olá, meu nome é ${this.nome} e eu tenho ${this.idade} anos.`);
  }
}

// Criando uma lista de pessoas
const pessoas = [
  new Pessoa("João", 25),
  new Pessoa("Maria", 30),
  new Pessoa("Pedro", 20),
  new Pessoa("Ana", 35)
];

// Função para encontrar a pessoa mais velha
function encontrarPessoaMaisVelha(pessoas) {
  let pessoaMaisVelha = null;
  for (let pessoa of pessoas) {
    if (!pessoaMaisVelha || pessoa.idade > pessoaMaisVelha.idade) {
      pessoaMaisVelha = pessoa;
    }
  }
  return pessoaMaisVelha;
}

// Função para filtrar pessoas com idade maior que um valor específico
function filtrarPessoasPorIdade(pessoas, idade) {
  return pessoas.filter(pessoa => pessoa.idade > idade);
}

// Chamando os métodos e imprimindo os resultados
encontrarPessoaMaisVelha(pessoas).saudacao();
console.log(filtrarPessoasPorIdade(pessoas, 25));
```

Neste código, começamos definindo uma classe chamada "Pessoa", que possui propriedades como nome e idade, e um método chamado "saudacao" que imprime uma mensagem de saudação com as informações da pessoa.

Em seguida, criamos uma lista de pessoas utilizando a classe "Pessoa" e instanciando objetos com diferentes nomes e idades.

Depois, temos a função "encontrarPessoaMaisVelha" que recebe a lista de pessoas como parâmetro e percorre a lista para encontrar a pessoa com a maior idade. O resultado é retornado como um objeto "Pessoa".

A função "filtrarPessoasPorIdade" recebe a lista de pessoas e uma idade como parâmetro, e utiliza o método "filter" para retornar um novo array contendo apenas as pessoas com idade maior que o valor informado.

Por fim, chamamos o método "saudacao" do objeto retornado pela função "encontrarPessoaMaisVelha" para imprimir a saudação da pessoa mais velha, e utilizamos a função "filtrarPessoasPorIdade" para filtrar e imprimir as pessoas com idade maior que 25 anos.

Espero que este código atenda às suas expectativas!