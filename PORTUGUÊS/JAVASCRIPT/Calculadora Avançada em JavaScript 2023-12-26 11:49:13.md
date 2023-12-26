Claro! Segue abaixo um exemplo de um código complexo em JavaScript:

```javascript
// Calculadora avançada em JavaScript
// Este código implementa as funções básicas de uma calculadora avançada,
// como adição, subtração, multiplicação, divisão, raiz quadrada e potenciação.

// Função para adicionar dois números
function adicionar(a, b) {
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
  if (b !== 0) {
    return a / b;
  } else {
    return "Erro: divisão por zero!";
  }
}

// Função para calcular a raiz quadrada de um número
function raizQuadrada(num) {
  if (num >= 0) {
    return Math.sqrt(num);
  } else {
    return "Erro: número negativo!";
  }
}

// Função para calcular a potência de um número
function potencia(base, expoente) {
  return Math.pow(base, expoente);
}

// Exemplo de uso das funções

// Adição de dois números
var resultadoAdicao = adicionar(5, 3);
console.log("Resultado da adição: " + resultadoAdicao);

// Subtração de dois números
var resultadoSubtracao = subtrair(10, 4);
console.log("Resultado da subtração: " + resultadoSubtracao);

// Multiplicação de dois números
var resultadoMultiplicacao = multiplicar(7, 2);
console.log("Resultado da multiplicação: " + resultadoMultiplicacao);

// Divisão de dois números
var resultadoDivisao = dividir(9, 3);
console.log("Resultado da divisão: " + resultadoDivisao);

// Cálculo da raiz quadrada de um número
var resultadoRaizQuadrada = raizQuadrada(25);
console.log("Resultado da raiz quadrada: " + resultadoRaizQuadrada);

// Cálculo da potência de um número
var resultadoPotencia = potencia(2, 4);
console.log("Resultado da potência: " + resultadoPotencia);
```

Explicação do código:
- O código implementa uma calculadora avançada em JavaScript, com funções para adicionar, subtrair, multiplicar, dividir, calcular a raiz quadrada e potenciação.
- Cada função recebe os parâmetros necessários e retorna o resultado do cálculo correspondente.
- O exemplo de uso das funções demonstra como chamar cada função e exibir o resultado no console.
- O código utiliza a função `console.log()` para exibir os resultados no console do navegador ou no console do Node.js.

Espero que este exemplo atenda às suas expectativas. Caso precise de mais detalhes ou outras funcionalidades, estou à disposição!