Claro! Aqui está um exemplo de um código complexo em JavaScript:

```javascript
// Calculadora Avançada

// Função para somar dois números
function somar(a, b) {
  return a + b;
}

// Função para subtrair dois números
function subtrair(a, b) {
  return a - b;
}

// Função para multiplicar dois números
function multiplicar(a, b) {
  return a * b;
}

// Função para dividir dois números
function dividir(a, b) {
  if (b === 0) {
    return "Erro: divisão por zero!";
  } else {
    return a / b;
  }
}

// Função para calcular o fatorial de um número
function fatorial(n) {
  if (n === 0) {
    return 1;
  } else {
    return n * fatorial(n - 1);
  }
}

// Função para verificar se um número é primo
function verificarPrimo(n) {
  if (n <= 1) {
    return false;
  } else if (n === 2) {
    return true;
  } else {
    for (let i = 2; i < n; i++) {
      if (n % i === 0) {
        return false;
      }
    }
    return true;
  }
}

// Função para calcular a sequência de Fibonacci
function fibonacci(n) {
  let fibonacciArray = [0, 1];
  for (let i = 2; i < n; i++) {
    fibonacciArray.push(fibonacciArray[i - 1] + fibonacciArray[i - 2]);
  }
  return fibonacciArray;
}

// Exemplo de uso das funções

console.log("Resultado da soma de 5 e 3:", somar(5, 3));
console.log("Resultado da subtração de 10 e 4:", subtrair(10, 4));
console.log("Resultado da multiplicação de 7 e 2:", multiplicar(7, 2));
console.log("Resultado da divisão de 20 por 5:", dividir(20, 5));
console.log("Resultado do fatorial de 5:", fatorial(5));
console.log("O número 7 é primo?", verificarPrimo(7));
console.log("Sequência de Fibonacci até o 10º elemento:", fibonacci(10));
```

Neste código, temos uma calculadora avançada com funcionalidades como somar, subtrair, multiplicar, dividir, calcular o fatorial de um número, verificar se um número é primo e calcular a sequência de Fibonacci.

As funções `somar`, `subtrair`, `multiplicar` e `dividir` são simples e realizam as operações matemáticas básicas. A função `dividir` inclui uma condição para evitar a divisão por zero, exibindo uma mensagem de erro caso isso ocorra.

A função `fatorial` utiliza uma recursão para calcular o fatorial de um número. O caso base é quando o número é igual a zero, retornando o valor 1. Caso contrário, a função chama a si mesma com um número menor e multiplica pelo número atual.

A função `verificarPrimo` verifica se um número é primo. Para isso, utiliza um loop `for` que itera de 2 até `n-1` e verifica se o número é divisível por algum número dentro deste intervalo.

A função `fibonacci` calcula a sequência de Fibonacci até o `n`-ésimo elemento. Utiliza um array para armazenar os valores da sequência, começando com os valores 0 e 1. Em seguida, utiliza um loop para calcular os próximos valores da sequência, somando os dois valores anteriores.

Por fim, o código faz uso das funções exibindo os resultados através da função `console.log`.