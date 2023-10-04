Claro! Vou criar um código em TypeScript que consiste em um algoritmo de busca binária recursiva. A busca binária é um algoritmo eficiente para encontrar um determinado valor em um array ordenado.

```typescript
function buscaBinariaRecursiva(array: number[], valor: number, inicio: number, fim: number): number {
    if (fim >= inicio) {
        const meio = Math.floor((inicio + fim) / 2);

        if (array[meio] === valor) {
            return meio;
        }

        if (array[meio] > valor) {
            return buscaBinariaRecursiva(array, valor, inicio, meio - 1);
        }

        return buscaBinariaRecursiva(array, valor, meio + 1, fim);
    }

    return -1;
}

const arrayOrdenado = [2, 5, 8, 12, 16, 23, 38, 56, 72, 91];
const valorProcurado = 23;
const indice = buscaBinariaRecursiva(arrayOrdenado, valorProcurado, 0, arrayOrdenado.length - 1);
console.log(`O valor ${valorProcurado} está no índice ${indice}.`);
```

Nesse código, temos uma função chamada `buscaBinariaRecursiva` que recebe um array ordenado, um valor a ser procurado, o índice de início e o índice de fim do intervalo de busca. Inicialmente, verificamos se o índice de fim é maior ou igual ao índice de início. Se for, calculamos o índice do elemento do meio do intervalo e comparamos com o valor procurado.

Se o elemento do meio for igual ao valor procurado, retornamos o índice correspondente. Caso contrário, verificamos se o elemento do meio é maior ou menor que o valor procurado. Se for maior, chamamos recursivamente a função de busca binária passando o intervalo da esquerda (do início até o meio - 1). Se for menor, chamamos recursivamente a função de busca binária passando o intervalo da direita (do meio + 1 até o fim).

Se o índice de fim for menor que o índice de início, significa que o valor não foi encontrado e retornamos -1.

No exemplo de uso, temos um array ordenado `[2, 5, 8, 12, 16, 23, 38, 56, 72, 91]` e queremos procurar o valor 23. Chamamos a função `buscaBinariaRecursiva` passando o array, o valor procurado, o índice inicial 0 e o índice final 9 (comprimento do array - 1). O retorno é o índice 5, que corresponde à posição do valor 23 no array.

Esse código utiliza a recursão para dividir o problema em subproblemas menores, tornando a busca binária eficiente e escalável para arrays maiores.