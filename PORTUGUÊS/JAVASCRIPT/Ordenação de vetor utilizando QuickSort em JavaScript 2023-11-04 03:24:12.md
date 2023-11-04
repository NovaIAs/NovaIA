Claro! Aqui está um exemplo de um código complexo em JavaScript que realiza a ordenação de um vetor utilizando o algoritmo QuickSort:

```javascript
function quickSort(vetor) {
  if (vetor.length <= 1) {
    return vetor;
  } else {
    var pivo = vetor[0];
    var menores = [];
    var maiores = [];

    for (var i = 1; i < vetor.length; i++) {
      if (vetor[i] <= pivo) {
        menores.push(vetor[i]);
      } else {
        maiores.push(vetor[i]);
      }
    }

    return quickSort(menores).concat(pivo, quickSort(maiores));
  }
}

function imprimirVetor(vetor) {
  console.log(`Vetor ordenado: [${vetor.join(", ")}]`);
}

var vetorDesordenado = [8, 3, 1, 5, 9, 2];
console.log(`Vetor original: [${vetorDesordenado.join(", ")}]`);

var vetorOrdenado = quickSort(vetorDesordenado);
imprimirVetor(vetorOrdenado);
```

Neste código, a função `quickSort` recebe um vetor como parâmetro e retorna o vetor ordenado utilizando o algoritmo de ordenação QuickSort.

A função `quickSort` possui uma condição de parada: se o vetor tiver tamanho menor ou igual a 1, ele já está ordenado e é retornado diretamente.

Caso contrário, a função seleciona um elemento do vetor como pivô (neste caso, o primeiro elemento) e cria dois vetores vazios: `menores` e `maiores`.

Em seguida, percorre-se o vetor a partir do segundo elemento e compara-se cada elemento com o pivô. Se o elemento for menor ou igual ao pivô, ele é adicionado ao vetor `menores`, caso contrário, ele é adicionado ao vetor `maiores`.

Depois disso, é feita a chamada recursiva da função `quickSort` para ordenar os vetores `menores` e `maiores`. Os resultados dessas chamadas recursivas são concatenados com o pivô e retornados como o vetor ordenado final.

A função `imprimirVetor` apenas recebe um vetor como parâmetro e imprime no console o vetor ordenado.

Por fim, é criado um vetor desordenado como exemplo `[8, 3, 1, 5, 9, 2]`, exibido o vetor original no console e chamada a função `quickSort` para ordená-lo. O resultado é impresso no console através da função `imprimirVetor`.