Aqui está um exemplo de um código complexo em JavaScript, que é um gerador de números primos:

```javascript
function gerarNumerosPrimos(n) {
  var numerosPrimos = [];
  
  for (var i = 2; i <= n; i++) {
    var ehPrimo = true;
    
    for (var j = 2; j <= Math.sqrt(i); j++) {
      if (i % j === 0) {
        ehPrimo = false;
        break;
      }
    }
    
    if (ehPrimo) {
      numerosPrimos.push(i);
    }
  }
  
  return numerosPrimos;
}

var limiteSuperior = 100;
var primos = gerarNumerosPrimos(limiteSuperior);

console.log("Números primos até " + limiteSuperior + ":");
console.log(primos);
```

Este código define uma função chamada `gerarNumerosPrimos` que recebe um número `n` como parâmetro e retorna um array contendo todos os números primos até `n`.

Em seguida, o código cria uma variável `limiteSuperior` com o valor `100` e chama a função `gerarNumerosPrimos` passando esse valor como argumento. O resultado é armazenado na variável `primos`.

Por fim, o código exibe uma mensagem no console indicando o limite superior usado e imprime o array de números primos.

A função `gerarNumerosPrimos` utiliza um algoritmo de força bruta para determinar se cada número é primo. Ele itera de `2` até `n` e, para cada número, verifica se é divisível por qualquer número entre `2` e a raiz quadrada dele (`Math.sqrt(i)`). Se encontrar um divisor, o número não é primo e a variável `ehPrimo` é definida como `false`. Caso contrário, o número é primo e é adicionado ao array `numerosPrimos`.

Esse algoritmo pode ser otimizado, mas o objetivo aqui é apenas fornecer um exemplo de código complexo em JavaScript.