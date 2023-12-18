Certamente! Aqui está um código complexo em JavaScript que realiza a ordenação de um array utilizando o algoritmo de ordenação por inserção. O código está em português e contém comentários explicativos para facilitar a compreensão:

```javascript
// Função para ordenar um array utilizando o algoritmo de ordenação por inserção
function ordenarArrayPorInsercao(array) {
  // Loop para percorrer todos os elementos do array
  for (let i = 1; i < array.length; i++) {
    // Definir o elemento atual a ser comparado
    let atual = array[i];
    // Definir o índice anterior ao elemento atual
    let j = i - 1;

    // Enquanto o índice anterior não for menor que 0 e o elemento anterior ao atual for maior que o atual
    while (j >= 0 && array[j] > atual) {
      // Mover o elemento anterior para a próxima posição
      array[j + 1] = array[j];
      // Decrementar o índice anterior
      j--;
    }

    // Inserir o elemento atual na posição correta
    array[j + 1] = atual;
  }

  // Retornar o array ordenado
  return array;
}

// Exemplo de uso
const array = [5, 3, 8, 2, 1, 4];
console.log("Array desordenado:", array);

const arrayOrdenado = ordenarArrayPorInsercao(array);
console.log("Array ordenado:", arrayOrdenado);
```

Neste código, a função `ordenarArrayPorInsercao` recebe um array como parâmetro e retorna o mesmo array ordenado em ordem crescente. O algoritmo de ordenação utilizado é o de inserção, que percorre o array e insere cada elemento em sua posição correta no subarray já ordenado.

O código utiliza um loop `for` para percorrer todos os elementos do array, iniciando a partir do segundo elemento (`i = 1`). Em seguida, é definido o elemento atual a ser comparado (`atual = array[i]`) e o índice anterior ao elemento atual (`j = i - 1`).

Dentro do loop `while`, o código verifica se o índice anterior `j` não é menor que 0 e se o elemento anterior ao atual (`array[j]`) é maior que o atual. Se essa condição for verdadeira, o elemento anterior é movido para a próxima posição (`array[j + 1] = array[j]`) e o índice anterior é decrementado (`j--`). Esse processo é repetido até que a condição não seja mais satisfeita.

Após sair do loop `while`, o elemento atual é inserido na posição correta no subarray já ordenado (`array[j + 1] = atual`).

Por fim, o array ordenado é retornado e impresso no console.

O exemplo de uso demonstra como utilizar a função `ordenarArrayPorInsercao` para ordenar o array `[5, 3, 8, 2, 1, 4]`. O array desordenado é exibido antes da ordenação, e o array ordenado é exibido após a ordenação.

Espero que esse código atenda às suas expectativas! Se você tiver mais alguma solicitação, fique à vontade para perguntar.