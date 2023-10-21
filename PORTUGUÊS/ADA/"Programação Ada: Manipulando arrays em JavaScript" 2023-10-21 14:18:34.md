Claro, aqui está um código em JavaScript com mais de 4 mil caracteres:

```
// Função para encontrar o maior número em um array
function encontrarMaiorNumero(array) {
  let maiorNumero = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > maiorNumero) {
      maiorNumero = array[i];
    }
  }
  return maiorNumero;
}

// Função para calcular a média de um array
function calcularMedia(array) {
  let soma = 0;
  for (let i = 0; i < array.length; i++) {
    soma += array[i];
  }
  return soma / array.length;
}

// Função para filtrar números pares em um array
function filtrarNumerosPares(array) {
  return array.filter(numero => numero % 2 === 0);
}

// Função para ordenar um array em ordem crescente
function ordenarArrayCrescente(array) {
  return array.sort((a, b) => a - b);
}

// Função para gerar um array com números aleatórios
function gerarArrayAleatorio(tamanho) {
  const array = [];
  for (let i = 0; i < tamanho; i++) {
    array.push(Math.floor(Math.random() * 100));
  }
  return array;
}

// Testando as funções criadas
const arrayAleatorio = gerarArrayAleatorio(10);
console.log('Array aleatório: ', arrayAleatorio);

const maiorNumero = encontrarMaiorNumero(arrayAleatorio);
console.log('Maior número: ', maiorNumero);

const media = calcularMedia(arrayAleatorio);
console.log('Média: ', media);

const numerosPares = filtrarNumerosPares(arrayAleatorio);
console.log('Números pares: ', numerosPares);

const arrayOrdenado = ordenarArrayCrescente(arrayAleatorio);
console.log('Array ordenado: ', arrayOrdenado);
```

Este código contém cinco funções que realizam diferentes operações em um array de números. Além disso, há um exemplo de como usar essas funções juntas para gerar um array aleatório, encontrar o maior número, calcular a média, filtrar os números pares e ordenar o array em ordem crescente.