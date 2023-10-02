Claro! Aqui está um código complexo em JavaScript que envolve uma série de operações matemáticas e manipulação de strings:

```javascript
// Função para verificar se uma string é um palíndromo
function verificarPalindromo(str) {
  // Remover espaços e converter para letras minúsculas
  str = str.replace(/\s/g, "").toLowerCase();
  
  // Inverter a string
  var reversed = str.split("").reverse().join("");
  
  // Verificar se a string invertida é igual à original
  if (str === reversed) {
    return true;
  } else {
    return false;
  }
}

// Função para calcular o fatorial de um número
function calcularFatorial(num) {
  if (num === 0 || num === 1) {
    return 1;
  } else {
    return num * calcularFatorial(num - 1);
  }
}

// Função para calcular o número de Fibonacci
function calcularFibonacci(n) {
  var fib = [0, 1];
  
  for (var i = 2; i <= n; i++) {
    fib[i] = fib[i - 1] + fib[i - 2];
  }
  
  return fib[n];
}

// Função para gerar um número aleatório entre dois valores
function gerarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Exemplo de uso das funções acima
var palavra = "arara";
var numero = 5;
var resultadoPalindromo = verificarPalindromo(palavra);
var resultadoFatorial = calcularFatorial(numero);
var resultadoFibonacci = calcularFibonacci(numero);
var numeroAleatorio = gerarNumeroAleatorio(1, 100);

console.log("A palavra '" + palavra + "' é um palíndromo? " + resultadoPalindromo);
console.log("O fatorial de " + numero + " é " + resultadoFatorial);
console.log("O número de Fibonacci na posição " + numero + " é " + resultadoFibonacci);
console.log("Número aleatório entre 1 e 100: " + numeroAleatorio);
```

Esse código contém quatro funções diferentes: `verificarPalindromo`, `calcularFatorial`, `calcularFibonacci` e `gerarNumeroAleatorio`. A função `verificarPalindromo` verifica se uma string é um palíndromo, a função `calcularFatorial` calcula o fatorial de um número, a função `calcularFibonacci` calcula o número de Fibonacci na posição especificada e a função `gerarNumeroAleatorio` gera um número aleatório entre dois valores.

O código também mostra exemplos de uso das funções, utilizando uma palavra para verificar se é um palíndromo, um número para calcular seu fatorial e seu número de Fibonacci, e também gera um número aleatório entre 1 e 100. Os resultados são exibidos no console.