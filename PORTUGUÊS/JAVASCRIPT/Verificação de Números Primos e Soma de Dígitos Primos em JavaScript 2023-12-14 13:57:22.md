Claro! Aqui está um código complexo em JavaScript que utiliza conceitos avançados de programação:

```javascript
// Função para verificar se um número é primo
function verificarPrimo(numero) {
  // Caso o número seja menor ou igual a 1, não é primo
  if (numero <= 1) {
    return false;
  }

  // Caso o número seja 2 ou 3, é primo
  if (numero == 2 || numero == 3) {
    return true;
  }

  // Caso o número seja divisível por 2 ou por 3, não é primo
  if (numero % 2 == 0 || numero % 3 == 0) {
    return false;
  }

  // Verifica se o número é divisível por qualquer número ímpar maior que 3
  // até a raiz quadrada do número
  for (let i = 5; i * i <= numero; i += 6) {
    if (numero % i == 0 || numero % (i + 2) == 0) {
      return false;
    }
  }

  // Se o número passou por todas as verificações, é primo
  return true;
}

// Função para calcular a soma dos dígitos de um número
function calcularSomaDigitos(numero) {
  let soma = 0;
  while (numero != 0) {
    soma += numero % 10;
    numero = Math.floor(numero / 10);
  }
  return soma;
}

// Função principal
function main() {
  let limiteInferior = 1;
  let limiteSuperior = 1000;

  let numerosPrimos = [];
  let numerosSomaDigitosPrimo = [];

  for (let i = limiteInferior; i <= limiteSuperior; i++) {
    if (verificarPrimo(i)) {
      numerosPrimos.push(i);

      let somaDigitos = calcularSomaDigitos(i);
      if (verificarPrimo(somaDigitos)) {
        numerosSomaDigitosPrimo.push(i);
      }
    }
  }

  console.log("Números primos no intervalo de", limiteInferior, "a", limiteSuperior + ":");
  console.log(numerosPrimos);

  console.log("Números cuja soma dos dígitos é primo no intervalo de", limiteInferior, "a", limiteSuperior + ":");
  console.log(numerosSomaDigitosPrimo);
}

// Executa a função principal
main();
```

Explicação do código:

1. A função `verificarPrimo` recebe um número como parâmetro e verifica se ele é primo ou não. Utiliza o algoritmo de Verificação de Primalidade por Divisão Sucessiva, que verifica se o número é divisível por números primos até a raiz quadrada do próprio número.

2. A função `calcularSomaDigitos` recebe um número como parâmetro e calcula a soma dos dígitos desse número. Utiliza um laço de repetição `while` para iterar pelos dígitos do número, obtendo o último dígito com o operador `%` e removendo-o com a divisão inteira por 10.

3. A função `main` é a função principal do programa. Define os limites inferior e superior do intervalo de números a serem verificados. Em seguida, cria dois arrays vazios para armazenar os números primos encontrados e os números cuja soma dos dígitos é primo.

4. Utiliza um laço de repetição `for` para iterar pelos números no intervalo definido. Para cada número, verifica se ele é primo utilizando a função `verificarPrimo`. Se for primo, adiciona-o ao array `numerosPrimos`.

5. Em seguida, calcula a soma dos dígitos do número utilizando a função `calcularSomaDigitos`. Verifica se a soma dos dígitos também é primo utilizando a função `verificarPrimo`. Se for primo, adiciona o número ao array `numerosSomaDigitosPrimo`.

6. Por fim, exibe os resultados no console, mostrando os números primos encontrados e os números cuja soma dos dígitos é primo.

Esse código verifica os números primos e os números cuja soma dos dígitos também é primo em um intervalo definido. É um exemplo de código complexo que utiliza funções, laços de repetição e algoritmos matemáticos.