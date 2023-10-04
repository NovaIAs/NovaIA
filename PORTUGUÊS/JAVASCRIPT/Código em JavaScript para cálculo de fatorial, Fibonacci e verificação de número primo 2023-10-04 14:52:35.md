Claro! Aqui está um exemplo de um código complexo em JavaScript:

```javascript
// Função para calcular o fatorial de um número
function calcularFatorial(numero) {
  if (numero === 0 || numero === 1) {
    return 1;
  } else {
    return numero * calcularFatorial(numero - 1);
  }
}

// Função para calcular o número de Fibonacci
function calcularFibonacci(n) {
  if (n === 0) {
    return 0;
  } else if (n === 1) {
    return 1;
  } else {
    return calcularFibonacci(n - 1) + calcularFibonacci(n - 2);
  }
}

// Função para verificar se um número é primo
function verificarPrimo(numero) {
  if (numero <= 1) {
    return false;
  } else if (numero === 2) {
    return true;
  } else if (numero % 2 === 0) {
    return false;
  } else {
    for (var i = 3; i <= Math.sqrt(numero); i += 2) {
      if (numero % i === 0) {
        return false;
      }
    }
    return true;
  }
}

// Exemplo de uso das funções
var numero = 5;
console.log("O fatorial de " + numero + " é " + calcularFatorial(numero));

var n = 10;
console.log("O " + n + "º número de Fibonacci é " + calcularFibonacci(n));

var num = 17;
if (verificarPrimo(num)) {
  console.log(num + " é um número primo.");
} else {
  console.log(num + " não é um número primo.");
}
```

Neste exemplo, temos três funções diferentes em JavaScript:

1. A função `calcularFatorial` recebe um número como parâmetro e retorna o fatorial desse número. Ela utiliza recursão para calcular o fatorial.
2. A função `calcularFibonacci` recebe um número como parâmetro e retorna o número correspondente na sequência de Fibonacci. Também utiliza recursão para calcular o número de Fibonacci.
3. A função `verificarPrimo` recebe um número como parâmetro e retorna um booleano indicando se o número é primo ou não. Ele utiliza um algoritmo de divisão para verificar se o número é divisível por algum número até a sua raiz quadrada.

No exemplo de uso das funções, definimos uma variável `numero` como 5 e chamamos a função `calcularFatorial` com esse valor. Em seguida, exibimos o resultado no console. Depois, definimos uma variável `n` como 10 e chamamos a função `calcularFibonacci` com esse valor, exibindo o resultado no console novamente. Por fim, definimos uma variável `num` como 17 e verificamos se é um número primo utilizando a função `verificarPrimo`, exibindo o resultado no console.

Esse código demonstra o uso de funções recursivas e algoritmos matemáticos para calcular o fatorial, a sequência de Fibonacci e verificar se um número é primo. Espero que este código atenda às suas expectativas!