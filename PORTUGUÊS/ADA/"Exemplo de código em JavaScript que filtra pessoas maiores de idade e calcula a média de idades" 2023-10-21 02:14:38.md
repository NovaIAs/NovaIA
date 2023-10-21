Claro, aqui está um exemplo de código complexo em JavaScript que envolve múltiplas funções e loops:

```javascript
// Cria um objeto com informações de uma pessoa
const pessoa = {
  nome: "João",
  idade: 30,
  profissao: "programador"
};

// Cria um array com informações de outras pessoas
const pessoas = [
  { nome: "Ana", idade: 25, profissao: "designer" },
  { nome: "Pedro", idade: 40, profissao: "engenheiro" },
  { nome: "Maria", idade: 35, profissao: "advogada" }
];

// Define uma função para verificar se uma pessoa é maior de idade
function maiorDeIdade(pessoa) {
  return pessoa.idade >= 18;
}

// Filtra as pessoas maiores de idade do array
const maioresDeIdade = pessoas.filter(maiorDeIdade);

// Define uma função para calcular a média de idades das pessoas
function mediaIdades(pessoas) {
  let somaIdades = 0;
  for (let i = 0; i < pessoas.length; i++) {
    somaIdades += pessoas[i].idade;
  }
  return somaIdades / pessoas.length;
}

// Calcula a média de idades das pessoas maiores de idade
const mediaIdadeMaiores = mediaIdades(maioresDeIdade);

// Define uma função para imprimir informações de uma pessoa
function imprimirPessoa(pessoa) {
  console.log(`${pessoa.nome} tem ${pessoa.idade} anos e é ${pessoa.profissao}.`);
}

// Imprime as informações de todas as pessoas maiores de idade
for (let i = 0; i < maioresDeIdade.length; i++) {
  imprimirPessoa(maioresDeIdade[i]);
}

// Imprime a média de idades das pessoas maiores de idade
console.log(`A média de idades das pessoas maiores de idade é ${mediaIdadeMaiores}.`);
```

Esse código cria um objeto com informações de uma pessoa, um array com informações de outras pessoas, define uma função para verificar se uma pessoa é maior de idade, filtra as pessoas maiores de idade do array, define uma função para calcular a média de idades das pessoas, calcula a média de idades das pessoas maiores de idade, define uma função para imprimir informações de uma pessoa, imprime as informações de todas as pessoas maiores de idade e imprime a média de idades das pessoas maiores de idade.