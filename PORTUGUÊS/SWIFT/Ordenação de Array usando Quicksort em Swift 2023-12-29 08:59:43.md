Certamente! Aqui está um exemplo de código em Swift que realiza a ordenação de um array usando o algoritmo de ordenação rápido, também conhecido como Quicksort:

```swift
func quickSort<T: Comparable>(_ array: inout [T], low: Int, high: Int) {
    if low < high {
        let pivotIndex = partition(&array, low: low, high: high)
        quickSort(&array, low: low, high: pivotIndex - 1)
        quickSort(&array, low: pivotIndex + 1, high: high)
    }
}

func partition<T: Comparable>(_ array: inout [T], low: Int, high: Int) -> Int {
    let pivot = array[high]
    var i = low
    for j in low..<high {
        if array[j] <= pivot {
            array.swapAt(i, j)
            i += 1
        }
    }
    array.swapAt(i, high)
    return i
}

var numbers = [5, 2, 9, 1, 7]
quickSort(&numbers, low: 0, high: numbers.count - 1)
print(numbers) // Resultado: [1, 2, 5, 7, 9]
```

Neste código, temos duas funções principais: `quickSort` e `partition`.

A função `quickSort` é responsável por realizar a ordenação do array. Ela recebe como parâmetros o array a ser ordenado, o índice inicial `low` e o índice final `high`. A função utiliza recursão para dividir o array em partições menores e ordená-las individualmente.

A função `partition` é utilizada pela função `quickSort` para particionar o array com base em um pivô. Ela recebe como parâmetros o array, o índice inicial `low` e o índice final `high`. A função percorre o array da esquerda para a direita, comparando cada elemento com o pivô. Se um elemento for menor ou igual ao pivô, ele é trocado com o elemento na posição correta, representada pela variável `i`. Ao final do processo, o pivô é colocado na posição correta e a função retorna o índice desse pivô.

No exemplo final, um array de números desordenados é passado para a função `quickSort`, juntamente com os índices iniciais e finais. O resultado é impresso no console e mostra o array ordenado em ordem crescente.

Espero que este exemplo atenda às suas expectativas!