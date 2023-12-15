Certamente! Aqui está um código complexo em Swift que envolve a manipulação de matrizes e a implementação de algoritmos de ordenação. O código a seguir é uma implementação do algoritmo Merge Sort, que ordena uma matriz de números inteiros em ordem crescente:

```swift
import Foundation

func mergeSort(_ array: [Int]) -> [Int] {
    guard array.count > 1 else {
        return array
    }
    
    let middleIndex = array.count / 2
    let leftArray = mergeSort(Array(array[..<middleIndex]))
    let rightArray = mergeSort(Array(array[middleIndex...]))
    
    return merge(leftArray, rightArray)
}

func merge(_ leftArray: [Int], _ rightArray: [Int]) -> [Int] {
    var leftIndex = 0
    var rightIndex = 0
    var mergedArray: [Int] = []
    
    while leftIndex < leftArray.count && rightIndex < rightArray.count {
        if leftArray[leftIndex] < rightArray[rightIndex] {
            mergedArray.append(leftArray[leftIndex])
            leftIndex += 1
        } else {
            mergedArray.append(rightArray[rightIndex])
            rightIndex += 1
        }
    }
    
    while leftIndex < leftArray.count {
        mergedArray.append(leftArray[leftIndex])
        leftIndex += 1
    }
    
    while rightIndex < rightArray.count {
        mergedArray.append(rightArray[rightIndex])
        rightIndex += 1
    }
    
    return mergedArray
}

let numbers = [9, 5, 2, 7, 3, 6, 8, 1, 4]
let sortedNumbers = mergeSort(numbers)
print("Números ordenados: \(sortedNumbers)")
```

Explicação do código:

1. Começamos importando o framework Foundation, que nos permite usar recursos adicionais do Swift.
2. Em seguida, temos a função `mergeSort`, que é a implementação principal do algoritmo Merge Sort. Ela recebe uma matriz de números inteiros como entrada e retorna a matriz ordenada.
3. Dentro da função `mergeSort`, verificamos se a matriz possui mais de um elemento. Se não, retornamos a matriz original, já que uma matriz com apenas um elemento já está ordenada.
4. Em seguida, é determinado o índice do meio da matriz e dividimos a matriz em duas partes, chamando recursivamente a função `mergeSort` para cada metade.
5. A função `merge` é responsável por mesclar as duas metades ordenadas da matriz em uma única matriz ordenada. Ela recebe duas matrizes como entrada e retorna a matriz mesclada.
6. Dentro da função `merge`, inicializamos as variáveis `leftIndex` e `rightIndex` como 0 para acompanhar os índices atuais de cada metade da matriz.
7. Utilizamos um loop while para comparar os elementos das matrizes `leftArray` e `rightArray`. Se o elemento da `leftArray` for menor que o elemento da `rightArray`, adicionamos o elemento à `mergedArray` e incrementamos `leftIndex`. Caso contrário, adicionamos o elemento da `rightArray` e incrementamos `rightIndex`.
8. Após o loop while, verificamos se ainda existem elementos restantes na `leftArray` ou na `rightArray` e adicionamos esses elementos à `mergedArray`.
9. Por fim, retornamos a `mergedArray`, que é a matriz ordenada resultante.
10. Na parte final do código, declaramos uma matriz de números desordenados, chamamos a função `mergeSort` passando essa matriz e, em seguida, imprimimos a matriz ordenada na saída.

Este código é apenas um exemplo de um algoritmo complexo implementado em Swift e pode ser adaptado para diferentes casos de uso.