Claro! Aqui está um exemplo de um código complexo em JavaScript que envolve manipulação de dados, funções e laços de repetição:

```javascript
// Função para verificar se um número é primo
function verificarPrimo(numero) {
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

// Função para somar os dígitos de um número
function somarDigitos(numero) {
  let soma = 0;

  while (numero > 0) {
    soma += numero % 10;
    numero = Math.floor(numero / 10);
  }

  return soma;
}

// Função principal
function main() {
  let numeros = [23, 45, 67, 89, 101, 123, 145];

  for (let i = 0; i < numeros.length; i++) {
    let numero = numeros[i];

    if (verificarPrimo(numero)) {
      console.log(`O número ${numero} é primo.`);
    } else {
      console.log(`O número ${numero} não é primo.`);
    }

    let soma = somarDigitos(numero);
    console.log(`A soma dos dígitos do número ${numero} é ${soma}.`);
  }
}

// Chamada da função principal
main();
```

Neste código, temos duas funções principais. A primeira função é `verificarPrimo(numero)`, que recebe um número como parâmetro e verifica se ele é primo ou não. A função utiliza um laço de repetição `for` para verificar se o número é divisível por algum outro número, exceto 1 e ele mesmo. Caso seja divisível, a função retorna `false`, indicando que o número não é primo. Caso contrário, retorna `true`.

A segunda função é `somarDigitos(numero)`, que recebe um número como parâmetro e calcula a soma de seus dígitos. A função utiliza um laço de repetição `while` para obter cada dígito do número, somando-os à variável `soma`. A cada iteração, o último dígito é removido dividindo o número por 10 e arredondando o resultado para baixo. A função retorna o valor da soma dos dígitos.

A função principal `main()` cria um array chamado `numeros` com uma lista de números. Em seguida, utiliza um laço `for` para percorrer cada número do array. Para cada número, chama as funções `verificarPrimo(numero)` e `somarDigitos(numero)`, exibindo os resultados no console.

Ao executar o código, será exibido no console se cada número é primo ou não, além da soma dos dígitos de cada número.