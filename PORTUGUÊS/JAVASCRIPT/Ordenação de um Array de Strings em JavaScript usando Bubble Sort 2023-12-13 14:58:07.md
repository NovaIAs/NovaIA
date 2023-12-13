Claro! Aqui está um código complexo em Javascript que envolve manipulação de arrays, laços de repetição e funções. Ele é um exemplo de algoritmo de ordenação chamado "Bubble Sort" e foi adaptado para trabalhar com strings em português.

```javascript
// Função para ordenar um array de strings em ordem alfabética
function ordenarArray(strings) {
  var trocou;
  do {
    trocou = false;
    for (var i = 0; i < strings.length - 1; i++) {
      if (strings[i].localeCompare(strings[i + 1]) > 0) {
        // Troca as posições se a string atual for maior que a próxima
        var temp = strings[i];
        strings[i] = strings[i + 1];
        strings[i + 1] = temp;
        trocou = true;
      }
    }
  } while (trocou);

  return strings;
}

// Array de strings desordenadas
var nomes = ["Maria", "Pedro", "Ana", "José", "Luísa", "Carlos"];

// Ordena o array
var nomesOrdenados = ordenarArray(nomes);

// Exibe o array ordenado
console.log("Nomes ordenados em ordem alfabética:");
for (var i = 0; i < nomesOrdenados.length; i++) {
  console.log(nomesOrdenados[i]);
}
```

Neste código, a função `ordenarArray()` recebe um array de strings como parâmetro e utiliza o algoritmo de ordenação Bubble Sort para ordenar as strings em ordem alfabética. 

A variável `trocou` é uma flag que indica se houve alguma troca de posições durante a iteração. O laço de repetição `do while` garante que o processo de ordenação seja realizado até que não ocorram mais trocas.

Dentro do laço `for`, o método `localeCompare()` é utilizado para comparar as strings. Se a string atual for maior que a próxima, as posições são trocadas.

Após ordenar o array, o código exibe os nomes ordenados em ordem alfabética no console.