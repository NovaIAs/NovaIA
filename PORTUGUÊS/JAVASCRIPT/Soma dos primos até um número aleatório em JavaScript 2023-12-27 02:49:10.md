Claro! Vou gerar um código complexo em JavaScript para você. Aqui está:

```javascript
// Função para verificar se um número é primo
function isPrimo(numero) {
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

// Função para encontrar a soma dos primos menores que um número
function somaPrimosAte(numero) {
  let soma = 0;
  
  for (let i = 2; i <= numero; i++) {
    if (isPrimo(i)) {
      soma += i;
    }
  }
  
  return soma;
}

// Função para gerar um número aleatório entre dois valores
function gerarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Função principal
function main() {
  const numero = gerarNumeroAleatorio(1, 100);
  const somaPrimos = somaPrimosAte(numero);
  
  console.log(`Número gerado: ${numero}`);
  console.log(`Soma dos primos menores que o número gerado: ${somaPrimos}`);
}

// Chamada da função principal
main();
```

Este código possui algumas funções que realizam operações matemáticas. A primeira função `isPrimo` verifica se um número é primo ou não, retornando `true` se for primo e `false` caso contrário.

A segunda função `somaPrimosAte` calcula a soma de todos os números primos menores ou iguais a um determinado número.

A terceira função `gerarNumeroAleatorio` gera um número aleatório dentro de um intervalo definido.

A função `main` é a função principal que realiza o seguinte procedimento:
1. Gera um número aleatório entre 1 e 100.
2. Calcula a soma dos números primos menores ou iguais ao número gerado.
3. Exibe o número gerado e a soma dos primos na saída do console.

Por fim, a função `main` é chamada para que todo o código seja executado.