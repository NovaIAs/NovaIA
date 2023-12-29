Claro! Aqui está um código complexo em JavaScript que encontra o maior número primo dentro de um determinado intervalo:

```javascript
function isPrimo(num) {
  if (num <= 1) {
    return false;
  } else if (num === 2) {
    return true;
  } else if (num % 2 === 0) {
    return false;
  }
  
  const limite = Math.floor(Math.sqrt(num));
  for (let i = 3; i <= limite; i += 2) {
    if (num % i === 0) {
      return false;
    }
  }
  
  return true;
}

function encontrarMaiorPrimo(inicio, fim) {
  let maiorPrimo = -1;
  
  for (let i = inicio; i <= fim; i++) {
    if (isPrimo(i) && i > maiorPrimo) {
      maiorPrimo = i;
    }
  }
  
  return maiorPrimo;
}

const intervaloInicio = 1;
const intervaloFim = 1000;
const maiorPrimo = encontrarMaiorPrimo(intervaloInicio, intervaloFim);

console.log(`O maior número primo entre ${intervaloInicio} e ${intervaloFim} é: ${maiorPrimo}`);
```

Neste código, temos duas funções principais. A primeira função, `isPrimo`, verifica se um número é primo ou não. Começamos tratando os casos especiais de números menores ou iguais a 1, retornando `false` para eles. Em seguida, verificamos se o número é igual a 2, retornando `true` nesse caso, pois 2 é um número primo. Por fim, percorremos todos os números ímpares a partir de 3 até a raiz quadrada do número em questão (arredondada para baixo), verificando se o número é divisível por algum deles. Se encontrarmos um divisor, o número não é primo e retornamos `false`. Caso contrário, o número é primo e retornamos `true`.

A segunda função, `encontrarMaiorPrimo`, recebe dois parâmetros: o início e o fim do intervalo em que queremos encontrar o maior número primo. Inicializamos a variável `maiorPrimo` com -1, pois ainda não encontramos nenhum número primo. Em seguida, percorremos todos os números dentro do intervalo, utilizando um laço `for`. Para cada número, chamamos a função `isPrimo` para verificar se é primo e, caso seja, verificamos se é maior que o valor armazenado em `maiorPrimo`. Se for, atualizamos o valor de `maiorPrimo` com o número encontrado.

Por fim, definimos os valores do intervalo (`intervaloInicio` e `intervaloFim`) e chamamos a função `encontrarMaiorPrimo` passando esses valores. O resultado é armazenado na variável `maiorPrimo` e é exibido no console.

Esse código encontra o maior número primo dentro de um intervalo especificado, permitindo a personalização do intervalo de busca.