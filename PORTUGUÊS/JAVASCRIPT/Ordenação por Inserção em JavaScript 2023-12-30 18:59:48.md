Claro! Aqui está um código complexo em JavaScript que faz a ordenação de um array usando o algoritmo de ordenação por inserção:

```javascript
function ordenacaoPorInsercao(array) {
  for (let i = 1; i < array.length; i++) {
    let chave = array[i];
    let j = i - 1;

    while (j >= 0 && array[j] > chave) {
      array[j + 1] = array[j];
      j--;
    }

    array[j + 1] = chave;
  }

  return array;
}

let array = [9, 5, 2, 7, 1];
console.log("Array original:", array);
console.log("Array ordenado:", ordenacaoPorInsercao(array));
```

Neste código, a função `ordenacaoPorInsercao` recebe um array como parâmetro e realiza a ordenação do mesmo utilizando o algoritmo de ordenação por inserção.

Primeiramente, é definido um loop `for` que percorre o array a partir do segundo elemento (índice 1). A cada iteração, o elemento atual é armazenado na variável `chave` e o índice `j` é definido como o índice anterior ao elemento atual.

Em seguida, é iniciado um loop `while` que verifica se o índice `j` é maior ou igual a 0 e se o elemento no índice `j` é maior do que a chave atual. Enquanto essa condição for verdadeira, o elemento no índice `j` é movido para a próxima posição (índice `j + 1`) e o índice `j` é decrementado.

Após sair do loop `while`, a chave é inserida na posição correta no array, ou seja, na próxima posição após o índice `j`.

Por fim, o array ordenado é retornado pela função e é feita a impressão do array original e do array ordenado no console.

Este algoritmo de ordenação por inserção é eficiente para arrays pequenos e parcialmente ordenados, mas pode se tornar lento para arrays grandes ou totalmente desordenados.