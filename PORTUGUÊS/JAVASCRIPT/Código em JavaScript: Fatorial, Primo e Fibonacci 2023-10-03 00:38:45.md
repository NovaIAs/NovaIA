Claro! Aqui está um código complexo em JavaScript:

```javascript
// Função recursiva para calcular o fatorial de um número
function fatorial(n) {
  if (n === 0 || n === 1) {
    return 1;
  } else {
    return n * fatorial(n - 1);
  }
}

// Função para verificar se um número é primo
function ehPrimo(num) {
  if (num <= 1) {
    return false;
  } else if (num === 2) {
    return true;
  } else if (num % 2 === 0) {
    return false;
  } else {
    for (let i = 3; i <= Math.sqrt(num); i += 2) {
      if (num % i === 0) {
        return false;
      }
    }
    return true;
  }
}

// Função para gerar a sequência de Fibonacci até um determinado limite
function gerarSequenciaFibonacci(limite) {
  let sequencia = [];
  let num1 = 0;
  let num2 = 1;
  while (num1 <= limite) {
    sequencia.push(num1);
    let proximoNum = num1 + num2;
    num1 = num2;
    num2 = proximoNum;
  }
  return sequencia;
}

// Exemplo de uso das funções
const numero = 10;
const resultadoFatorial = fatorial(numero);
console.log(`O fatorial de ${numero} é ${resultadoFatorial}`);

const numeroPrimo = 17;
const resultadoPrimo = ehPrimo(numeroPrimo);
console.log(`O número ${numeroPrimo} é primo? ${resultadoPrimo}`);

const limiteFibonacci = 100;
const sequenciaFibonacci = gerarSequenciaFibonacci(limiteFibonacci);
console.log(`Sequência de Fibonacci até ${limiteFibonacci}: ${sequenciaFibonacci}`);
```

Explicação do código:

1. A função `fatorial` é uma função recursiva que calcula o fatorial de um número. Ela verifica se o número é 0 ou 1, retornando 1 nesses casos, e caso contrário, chama a função `fatorial` novamente passando como argumento o número decrementado em 1 e multiplica pelo próprio número.

2. A função `ehPrimo` verifica se um número é primo. Ela primeiro faz algumas verificações específicas para os números 0, 1 e 2. Para números maiores que 2, ela utiliza um loop para verificar se o número é divisível por algum outro número até a sua raiz quadrada. Se for divisível, retorna falso, caso contrário, retorna verdadeiro.

3. A função `gerarSequenciaFibonacci` gera a sequência de Fibonacci até um determinado limite. Ela utiliza um loop while para ir gerando os números da sequência. A cada iteração, adiciona o número atual na sequência e calcula o próximo número somando os dois números anteriores.

No exemplo de uso das funções, são definidos alguns valores de teste. O código então calcula o fatorial de um número, verifica se um número é primo e gera a sequência de Fibonacci até um determinado limite. Os resultados são exibidos no console utilizando o `console.log()`.

Espero que isso atenda às suas necessidades! Se você tiver alguma dúvida ou precisar de mais informações, é só me perguntar.