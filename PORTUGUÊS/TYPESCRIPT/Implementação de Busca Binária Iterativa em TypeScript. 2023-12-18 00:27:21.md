Claro! Aqui está um exemplo de um código complexo em TypeScript que implementa um algoritmo de busca binária iterativa:

```typescript
function buscaBinariaIterativa(array: number[], valor: number): number {
  let inicio = 0;
  let fim = array.length - 1;

  while (inicio <= fim) {
    const meio = Math.floor((inicio + fim) / 2);

    if (array[meio] === valor) {
      return meio;
    } else if (array[meio] < valor) {
      inicio = meio + 1;
    } else {
      fim = meio - 1;
    }
  }

  return -1;
}

const valores = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
const valorBusca = 7;

const indice = buscaBinariaIterativa(valores, valorBusca);

if (indice !== -1) {
  console.log(`O valor ${valorBusca} foi encontrado no índice ${indice}.`);
} else {
  console.log(`O valor ${valorBusca} não foi encontrado no array.`);
}
```

Neste código, temos uma função chamada `buscaBinariaIterativa` que recebe um array de números e um valor a ser buscado. A função implementa a busca binária iterativa, que divide o array em duas partes e verifica se o valor procurado está na parte esquerda ou direita.

A função utiliza duas variáveis `inicio` e `fim` para marcar os índices de início e fim do intervalo de busca. O algoritmo então calcula o índice do elemento do meio do intervalo usando a fórmula `(inicio + fim) / 2` e arredonda para baixo usando `Math.floor`.

Em seguida, o algoritmo compara o valor no índice do meio com o valor buscado. Se forem iguais, o algoritmo retorna o índice do meio, indicando que o valor foi encontrado. Se o valor no índice do meio for menor que o valor buscado, o algoritmo atualiza o índice de início para o meio + 1, indicando que a busca deve continuar na parte direita do intervalo. Caso contrário, o algoritmo atualiza o índice de fim para o meio - 1, indicando que a busca deve continuar na parte esquerda do intervalo.

O algoritmo repete esse processo enquanto o índice de início for menor ou igual ao índice de fim. Se o algoritmo não encontrar o valor buscado, ele retorna -1.

No exemplo do código, o array `valores` contém os números de 1 a 10 em ordem crescente. A variável `valorBusca` guarda o valor 7, que será buscado no array utilizando a função `buscaBinariaIterativa`. O resultado da busca é armazenado na variável `indice`, que é então verificada para determinar se o valor foi encontrado ou não, exibindo a mensagem apropriada no console.