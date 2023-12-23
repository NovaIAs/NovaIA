Claro! Aqui está um código em Swift que implementa um algoritmo de ordenação chamado Bubble Sort. O Bubble Sort é um algoritmo simples, porém, eficiente para pequenas listas. O código a seguir ordena uma lista de números inteiros em ordem crescente:

```swift
func bubbleSort(_ array: inout [Int]) {
    let n = array.count
    var trocado: Bool

    for i in 0..<n - 1 {
        trocado = false

        for j in 0..<n - i - 1 {
            if array[j] > array[j + 1] {
                array.swapAt(j, j + 1)
                trocado = true
            }
        }

        if !trocado {
            break
        }
    }
}

var numeros = [12, 5, 89, 3, 56, 2, 10]
print("Lista original: \(numeros)")

bubbleSort(&numeros)
print("Lista ordenada: \(numeros)")
```

Explicação do código:

1. A função `bubbleSort` recebe uma lista de números inteiros como parâmetro e utiliza o modificador `inout` para permitir a modificação direta do array.

2. A variável `n` armazena o tamanho da lista.

3. A variável `trocado` é usada para verificar se ocorreu alguma troca durante a iteração. Se nenhuma troca ocorreu em uma passagem completa, isso significa que a lista já está ordenada e o algoritmo pode ser interrompido.

4. O primeiro loop `for` itera por cada elemento da lista, exceto o último.

5. Dentro do primeiro loop, o segundo loop `for` compara cada elemento adjacente e realiza a troca se o elemento atual for maior que o próximo elemento.

6. Se ocorrer uma troca, a variável `trocado` é definida como `true`.

7. Após o segundo loop, é verificado se ocorreu alguma troca. Se não houve troca, o algoritmo é interrompido, pois a lista já está ordenada.

8. Por fim, a função `bubbleSort` é chamada passando a lista `numeros` como argumento e o resultado é impresso na tela.

Espero que esse código atenda às suas expectativas! Se tiver mais alguma solicitação, não hesite em pedir.