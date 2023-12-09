Claro! Aqui está um exemplo de um código complexo em JavaScript que realiza uma série de operações matemáticas envolvendo manipulação de arrays:

```javascript
// Função para gerar um array com números pares de 2 até um número máximo
function gerarArrayNumerosPares(max) {
  const numerosPares = [];

  for (let i = 2; i <= max; i += 2) {
    numerosPares.push(i);
  }

  return numerosPares;
}

// Função para elevar ao quadrado os números de um array
function elevarAoQuadrado(array) {
  const arrayAoQuadrado = [];

  for (let i = 0; i < array.length; i++) {
    const numeroAoQuadrado = array[i] ** 2;
    arrayAoQuadrado.push(numeroAoQuadrado);
  }

  return arrayAoQuadrado;
}

// Função para somar todos os números de um array
function somarArray(array) {
  let soma = 0;

  for (let i = 0; i < array.length; i++) {
    soma += array[i];
  }

  return soma;
}

// Função para calcular a média dos números de um array
function calcularMedia(array) {
  const soma = somarArray(array);
  const media = soma / array.length;

  return media;
}

// Função para encontrar o maior número de um array
function encontrarMaiorNumero(array) {
  let maiorNumero = array[0];

  for (let i = 1; i < array.length; i++) {
    if (array[i] > maiorNumero) {
      maiorNumero = array[i];
    }
  }

  return maiorNumero;
}

// Gerar um array com números pares de 2 até 10
const numerosPares = gerarArrayNumerosPares(10);

// Elevar ao quadrado os números do array
const numerosAoQuadrado = elevarAoQuadrado(numerosPares);

// Somar todos os números do array
const somaDosNumeros = somarArray(numerosAoQuadrado);

// Calcular a média dos números do array
const mediaDosNumeros = calcularMedia(numerosAoQuadrado);

// Encontrar o maior número do array
const maiorNumero = encontrarMaiorNumero(numerosAoQuadrado);

console.log("Array de números pares:", numerosPares);
console.log("Array elevado ao quadrado:", numerosAoQuadrado);
console.log("Soma dos números:", somaDosNumeros);
console.log("Média dos números:", mediaDosNumeros);
console.log("Maior número:", maiorNumero);
```

Neste código, temos várias funções que realizam diferentes operações matemáticas em arrays. A função `gerarArrayNumerosPares` cria um array contendo números pares de 2 até um número máximo especificado. A função `elevarAoQuadrado` recebe um array e retorna um novo array com os números elevados ao quadrado. A função `somarArray` calcula a soma de todos os números de um array. A função `calcularMedia` calcula a média dos números de um array. E a função `encontrarMaiorNumero` encontra o maior número de um array.

No final do código, são chamadas as funções para realizar as operações desejadas. O resultado é exibido no console por meio de chamadas à função `console.log`.