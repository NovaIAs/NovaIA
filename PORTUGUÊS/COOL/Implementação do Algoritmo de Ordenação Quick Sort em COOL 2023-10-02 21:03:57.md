Claro, vou gerar um código em COOL que exemplifica a implementação de um algoritmo de ordenação chamado Quick Sort. O Quick Sort é um algoritmo de ordenação eficiente que utiliza a estratégia de dividir para conquistar.

Em COOL, o código seria assim:

```
class QuickSort {
    // Método para ordenar o array
    method sort(arr: ARRAY[INT]) : ARRAY[INT] {
        if length(arr) <= 1 then
            return arr
        else {
            // Seleciona um elemento como pivô (geralmente o último elemento)
            let pivot : INT <- arr[length(arr) - 1]
            var smaller : ARRAY[INT] <- []
            var equal : ARRAY[INT] <- []
            var greater : ARRAY[INT] <- []

            // Divide o array em três partes: menores, iguais e maiores que o pivô
            for i : INT in arr {
                if i < pivot then
                    smaller <- smaller + [i]
                elseif i = pivot then
                    equal <- equal + [i]
                else
                    greater <- greater + [i]
                fi
            }

            // Ordena as partes menores e maiores recursivamente
            smaller <- sort(smaller)
            greater <- sort(greater)

            // Concatena as partes ordenadas
            return smaller + equal + greater
        }
    }
}

// Exemplo de uso
let arr : ARRAY[INT] <- [5, 2, 9, 1, 7]
let sortedArr : ARRAY[INT] <- QuickSort.sort(arr)
```

Neste código, a classe `QuickSort` possui um único método chamado `sort`, que recebe um array de inteiros e retorna o array ordenado. O método utiliza a estratégia de dividir o array em três partes: menores, iguais e maiores que o pivô. Em seguida, ele ordena recursivamente as partes menores e maiores, e concatena as partes ordenadas.

Para testar o código, é criado um array de exemplo `arr` com alguns números desordenados. Em seguida, é chamado o método `sort` da classe `QuickSort`, passando `arr` como argumento, e o resultado é armazenado na variável `sortedArr`. Após a execução, `sortedArr` conterá o array `arr` ordenado.

O Quick Sort é um algoritmo de ordenação popular e eficiente, sendo muito utilizado na prática. Ele possui uma complexidade média de O(n log n), tornando-o uma opção viável para ordenar grandes conjuntos de dados.