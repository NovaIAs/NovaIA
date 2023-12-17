Claro! Aqui está um código complexo em JavaScript que envolve diversas funcionalidades e é explicado em detalhes:

```javascript
// Função para calcular o fatorial de um número
function fatorial(n) {
  if (n <= 1) {
    return 1;
  } else {
    return n * fatorial(n - 1);
  }
}

// Função para verificar se um número é primo
function ehPrimo(n) {
  if (n <= 1) {
    return false;
  }
  for (let i = 2; i < n; i++) {
    if (n % i === 0) {
      return false;
    }
  }
  return true;
}

// Classe que representa uma Pessoa
class Pessoa {
  constructor(nome, idade) {
    this.nome = nome;
    this.idade = idade;
  }

  saudacao() {
    console.log(`Olá, meu nome é ${this.nome} e eu tenho ${this.idade} anos.`);
  }
}

// Função para encontrar o maior valor em um array
function encontrarMaiorValor(array) {
  let maior = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > maior) {
      maior = array[i];
    }
  }
  return maior;
}

// Exemplo de uso das funções e classe

// Cálculo do fatorial de 5
const fatorialDeCinco = fatorial(5);
console.log(`O fatorial de 5 é ${fatorialDeCinco}.`);

// Verificação se o número 7 é primo
const numeroSeteEhPrimo = ehPrimo(7);
console.log(`O número 7 é primo? ${numeroSeteEhPrimo}.`);

// Criação de uma instância da classe Pessoa
const pessoa = new Pessoa("João", 30);
pessoa.saudacao();

// Encontrar o maior valor em um array
const numeros = [1, 5, 3, 9, 2, 6];
const maiorValor = encontrarMaiorValor(numeros);
console.log(`O maior valor no array [${numeros}] é ${maiorValor}.`);
```

Nesse código, temos algumas funcionalidades interessantes:

1. A função `fatorial(n)` calcula o fatorial de um número utilizando uma recursão.
2. A função `ehPrimo(n)` verifica se um número é primo, percorrendo todos os números de 2 até `n-1` e verificando se o número é divisível por algum deles.
3. A classe `Pessoa` representa uma pessoa com nome e idade, e possui um método `saudacao()` que imprime uma saudação utilizando as propriedades da instância.
4. A função `encontrarMaiorValor(array)` percorre um array de números e retorna o maior valor encontrado.

No exemplo de uso, o código realiza as seguintes ações:

1. Calcula o fatorial de 5 e imprime o resultado.
2. Verifica se o número 7 é primo e imprime o resultado.
3. Cria uma instância da classe `Pessoa` com nome "João" e idade 30, e chama o método `saudacao()`.
4. Encontra o maior valor no array `[1, 5, 3, 9, 2, 6]` e imprime o resultado.

Esse código é apenas um exemplo de um código complexo em JavaScript, com diversas funcionalidades e uma explicação detalhada. Lembre-se de adaptá-lo às suas necessidades e adicionar as funcionalidades que você precisa.