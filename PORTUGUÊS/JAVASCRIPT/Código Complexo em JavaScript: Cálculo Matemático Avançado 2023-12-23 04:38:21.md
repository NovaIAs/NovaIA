Claro! Aqui está um exemplo de um código complexo em JavaScript que realiza um cálculo matemático avançado. O código utiliza conceitos de funções, loops e manipulação de arrays:

```javascript
// Função para gerar um número aleatório entre um intervalo
function gerarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Função para verificar se um número é primo
function verificarNumeroPrimo(numero) {
  if (numero <= 1) {
    return false;
  }
  
  for (let i = 2; i < numero; i++) {
    if (numero % i === 0) {
      return false;
    }
  }
  
  return true;
}

// Função para calcular o fatorial de um número
function calcularFatorial(numero) {
  let fatorial = 1;
  
  for (let i = 2; i <= numero; i++) {
    fatorial *= i;
  }
  
  return fatorial;
}

// Função para calcular a soma dos dígitos de um número
function calcularSomaDigitos(numero) {
  let soma = 0;
  
  while (numero > 0) {
    soma += numero % 10;
    numero = Math.floor(numero / 10);
  }
  
  return soma;
}

// Função principal
function main() {
  const numeros = [];
  
  // Gerar 10 números aleatórios entre 1 e 100
  for (let i = 0; i < 10; i++) {
    const numero = gerarNumeroAleatorio(1, 100);
    numeros.push(numero);
  }
  
  // Verificar se cada número é primo e calcular seu fatorial e a soma dos dígitos
  for (let i = 0; i < numeros.length; i++) {
    const numero = numeros[i];
    
    const ehPrimo = verificarNumeroPrimo(numero);
    const fatorial = calcularFatorial(numero);
    const somaDigitos = calcularSomaDigitos(numero);
    
    console.log(`Número: ${numero}`);
    console.log(`É primo? ${ehPrimo}`);
    console.log(`Fatorial: ${fatorial}`);
    console.log(`Soma dos dígitos: ${somaDigitos}`);
    console.log('---');
  }
}

// Chamar a função principal
main();
```

Neste código, temos algumas funções principais. A função `gerarNumeroAleatorio` recebe um valor mínimo e máximo, utiliza a função `Math.random()` para gerar um número aleatório entre esses valores e retorna o número gerado.

A função `verificarNumeroPrimo` recebe um número como parâmetro e verifica se ele é primo ou não. Para isso, utiliza um loop que divide o número por todos os valores entre 2 e o número - 1. Se o número for divisível por algum desses valores, então não é primo.

A função `calcularFatorial` recebe um número como parâmetro e calcula o fatorial desse número utilizando um loop que multiplica o número por todos os valores entre 2 e o próprio número.

A função `calcularSomaDigitos` recebe um número como parâmetro e calcula a soma dos dígitos desse número utilizando um loop que extrai o último dígito do número e o adiciona à soma, até que o número se torne zero.

A função `main` é a função principal que realiza as chamadas das outras funções. Nela, geramos 10 números aleatórios entre 1 e 100, e para cada número gerado, verificamos se é primo, calculamos seu fatorial e a soma dos dígitos, e exibimos os resultados no console.

Por fim, chamamos a função `main` para iniciar a execução do programa.