Claro! Aqui está um código em TypeScript que realiza a ordenação de um array de números usando o algoritmo Bubble Sort:

```typescript
function bubbleSort(array: number[]): number[] {
  const length = array.length;

  for (let i = 0; i < length - 1; i++) {
    for (let j = 0; j < length - i - 1; j++) {
      if (array[j] > array[j + 1]) {
        const temp = array[j];
        array[j] = array[j + 1];
        array[j + 1] = temp;
      }
    }
  }

  return array;
}

const unsortedArray = [5, 2, 8, 12, 1];
const sortedArray = bubbleSort(unsortedArray);

console.log(sortedArray);
```

Neste código, temos uma função chamada `bubbleSort` que recebe um array de números como parâmetro e retorna o mesmo array ordenado de forma crescente.

A função implementa o algoritmo Bubble Sort, que consiste em percorrer o array várias vezes, comparando elementos adjacentes e trocando-os de posição caso estejam na ordem errada. O processo é repetido até que nenhum elemento precise ser trocado.

O código possui dois loops aninhados. O loop externo (`for`) percorre o array a partir do primeiro elemento até o penúltimo elemento. O loop interno também percorre o array, mas seu limite é atualizado a cada iteração do loop externo, garantindo que os elementos já ordenados na última posição sejam ignorados.

Dentro do loop interno, há uma condição (`if`) que verifica se o elemento atual é maior que o próximo elemento. Se for, ocorre a troca dos elementos utilizando uma variável temporária.

Após a ordenação, o array ordenado é retornado pela função `bubbleSort` e em seguida é exibido no console através do `console.log`.

Esse é um exemplo de código em TypeScript que utiliza o algoritmo Bubble Sort para ordenar um array de números. É importante ressaltar que existem outros algoritmos de ordenação mais eficientes, mas o Bubble Sort foi escolhido para fins explicativos.