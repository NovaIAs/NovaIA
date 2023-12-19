Claro! Aqui está um exemplo de um código complexo em TypeScript, que implementa um algoritmo de busca binária recursiva para encontrar um elemento em um array ordenado:

```typescript
function buscaBinariaRecursiva(array: number[], elemento: number, inicio: number, fim: number): number {
  if (inicio > fim) {
    return -1; // Elemento não encontrado
  }

  const meio = Math.floor((inicio + fim) / 2);

  if (array[meio] === elemento) {
    return meio; // Elemento encontrado
  } else if (array[meio] < elemento) {
    return buscaBinariaRecursiva(array, elemento, meio + 1, fim); // Busca na metade direita do array
  } else {
    return buscaBinariaRecursiva(array, elemento, inicio, meio - 1); // Busca na metade esquerda do array
  }
}

const arrayOrdenado = [2, 5, 8, 12, 16, 23, 38, 56, 72];
const elementoProcurado = 23;
const indiceEncontrado = buscaBinariaRecursiva(arrayOrdenado, elementoProcurado, 0, arrayOrdenado.length - 1);

if (indiceEncontrado !== -1) {
  console.log(`O elemento ${elementoProcurado} foi encontrado no índice ${indiceEncontrado}.`);
} else {
  console.log(`O elemento ${elementoProcurado} não foi encontrado no array.`);
}
```

Neste exemplo, a função `buscaBinariaRecursiva` é responsável por realizar a busca binária de forma recursiva. Ela recebe como parâmetros o `array` no qual realizar a busca, o `elemento` que se deseja encontrar, o `inicio` e o `fim` do intervalo de busca.

A função verifica se o `inicio` é maior do que o `fim`, o que indica que o `elemento` não foi encontrado, retornando -1 nesse caso.

Caso contrário, a função calcula o `meio` do intervalo de busca através da média aritmética inteira entre o `inicio` e o `fim`. Em seguida, compara o elemento do `array` na posição `meio` com o `elemento` procurado:

- Se forem iguais, o `elemento` foi encontrado e a função retorna o índice `meio`.
- Se o elemento do `array` for menor do que o `elemento` procurado, a função chama recursivamente a si mesma, passando o intervalo da metade direita do `array` como novo intervalo de busca.
- Se o elemento do `array` for maior do que o `elemento` procurado, a função chama recursivamente a si mesma, passando o intervalo da metade esquerda do `array` como novo intervalo de busca.

Após a chamada recursiva, o código principal utiliza a função `buscaBinariaRecursiva` para procurar o `elementoProcurado` no `arrayOrdenado`. Em seguida, verifica se o `indiceEncontrado` é diferente de -1, o que indica que o `elemento` foi encontrado. Caso seja diferente de -1, exibe uma mensagem informando o índice onde o `elemento` foi encontrado. Caso contrário, exibe uma mensagem informando que o `elemento` não foi encontrado no `array`.