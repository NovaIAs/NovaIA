Claro! Aqui está um exemplo de um código complexo em JavaScript:

```javascript
// Função para verificar se um número é primo
function verificarPrimo(numero) {
  // Verifica se o número é menor ou igual a 1
  if (numero <= 1) {
    return false;
  }

  // Verifica se o número é divisível por algum número entre 2 e a raiz quadrada do número
  for (let i = 2; i <= Math.sqrt(numero); i++) {
    if (numero % i === 0) {
      return false;
    }
  }

  // Se não for divisível por nenhum número, é primo
  return true;
}

// Função para calcular a sequência de Fibonacci
function calcularFibonacci(n) {
  let fibonacci = [0, 1];

  for (let i = 2; i < n; i++) {
    fibonacci[i] = fibonacci[i - 1] + fibonacci[i - 2];
  }

  return fibonacci;
}

// Função para ordenar um array de números usando o algoritmo Bubble Sort
function bubbleSort(array) {
  let trocado;
  do {
    trocado = false;
    for (let i = 0; i < array.length - 1; i++) {
      if (array[i] > array[i + 1]) {
        let temp = array[i];
        array[i] = array[i + 1];
        array[i + 1] = temp;
        trocado = true;
      }
    }
  } while (trocado);

  return array;
}

// Exemplo de uso das funções
let numero = 17;
console.log(`O número ${numero} é primo? ${verificarPrimo(numero)}`);

let n = 10;
console.log(`Os primeiros ${n} números da sequência de Fibonacci são: ${calcularFibonacci(n)}`);

let array = [5, 2, 8, 1, 9];
console.log(`Array antes da ordenação: ${array}`);
console.log(`Array após a ordenação usando Bubble Sort: ${bubbleSort(array)}`);
```

Neste código, temos três funções principais:

1. A função `verificarPrimo(numero)` verifica se um número é primo. Ela recebe um número como parâmetro e retorna `true` se o número for primo, ou `false` caso contrário.

2. A função `calcularFibonacci(n)` calcula os primeiros `n` números da sequência de Fibonacci. Ela recebe um número `n` como parâmetro e retorna um array contendo os números da sequência.

3. A função `bubbleSort(array)` ordena um array de números utilizando o algoritmo Bubble Sort. Ela recebe um array como parâmetro e retorna o array ordenado.

No exemplo de uso das funções, é definido um número para verificar se é primo, um número para calcular a sequência de Fibonacci e um array para ser ordenado. Os resultados são exibidos no console.

Espero que este código atenda às suas expectativas!