Claro! Aqui está um código complexo em JavaScript que realiza uma busca binária em um array ordenado de números. A busca binária é um algoritmo eficiente que divide repetidamente o intervalo de busca pela metade, reduzindo assim o número de elementos a serem verificados.

```javascript
function buscaBinaria(array, valorProcurado) {
    let inicio = 0;
    let fim = array.length - 1;

    while (inicio <= fim) {
        let meio = Math.floor((inicio + fim) / 2);

        if (array[meio] === valorProcurado) {
            return meio;  // Encontrou o valor, retorna o índice
        } else if (array[meio] < valorProcurado) {
            inicio = meio + 1;  // Valor está à direita do meio
        } else {
            fim = meio - 1;  // Valor está à esquerda do meio
        }
    }

    return -1;  // Valor não encontrado no array
}

// Exemplo de uso
const numeros = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19];
const valorProcurado = 11;

const resultado = buscaBinaria(numeros, valorProcurado);

if (resultado !== -1) {
    console.log(`O valor ${valorProcurado} foi encontrado no índice ${resultado}.`);
} else {
    console.log(`O valor ${valorProcurado} não foi encontrado.`);
}
```

Explicação do código:

- A função `buscaBinaria` recebe dois parâmetros: `array` (o array ordenado em que a busca será realizada) e `valorProcurado` (o valor que estamos procurando no array).
- Inicialmente, definimos dois ponteiros: `inicio` com valor 0 e `fim` com o índice do último elemento do array.
- O algoritmo utiliza um loop `while` para repetir o processo até que `inicio` seja maior que `fim`. Isso significa que a busca foi concluída e o valor não foi encontrado.
- Em cada iteração do loop, calculamos o índice do elemento do meio através da fórmula `(inicio + fim) / 2`. Usamos `Math.floor` para garantir que o número seja inteiro.
- Comparamos o valor do elemento do meio com o `valorProcurado`. Se forem iguais, retornamos o índice.
- Caso contrário, se o valor do elemento do meio for menor que `valorProcurado`, atualizamos `inicio` para `meio + 1`, indicando que a busca deve ser realizada na metade direita do array.
- Por fim, se o valor do elemento do meio for maior que `valorProcurado`, atualizamos `fim` para `meio - 1`, indicando que a busca deve ser realizada na metade esquerda do array.
- Se o valor não for encontrado após o loop, retornamos -1.
- No exemplo de uso, definimos um array ordenado `numeros` e um valor a ser procurado `valorProcurado`.
- Chamamos a função `buscaBinaria` passando o array e o valor como argumentos.
- Verificamos o resultado retornado pela função. Se for diferente de -1, exibimos a mensagem informando o índice onde o valor foi encontrado. Caso contrário, exibimos a mensagem informando que o valor não foi encontrado.