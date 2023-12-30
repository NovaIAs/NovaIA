Certamente! Aqui está um exemplo de código em Swift que realiza a ordenação de uma lista de números usando o algoritmo Bubble Sort:

```swift
func bubbleSort(_ array: inout [Int]) {
    let n = array.count
    var trocou = false
    
    for i in 0..<n-1 {
        for j in 0..<n-i-1 {
            if array[j] > array[j+1] {
                array.swapAt(j, j+1)
                trocou = true
            }
        }
        
        if !trocou {
            break
        }
    }
}

var numeros = [5, 2, 8, 1, 9]

bubbleSort(&numeros)

print("Lista ordenada: \(numeros)")
```

Neste código, a função `bubbleSort` recebe um array de números como parâmetro e realiza a ordenação utilizando o algoritmo Bubble Sort. 

Primeiro, é obtido o tamanho do array, armazenado na constante `n`. A variável `trocou` é inicializada como `false` e será utilizada para verificar se ocorreu alguma troca durante a iteração.

Em seguida, são percorridos dois loops: o primeiro loop `for i` controla as iterações de passos e o segundo loop `for j` percorre os elementos do array. A cada iteração do segundo loop, comparamos o elemento atual `array[j]` com o próximo elemento `array[j+1]`. Se o elemento atual for maior que o próximo, realizamos a troca dos elementos utilizando o método `swapAt` do array e atualizamos a variável `trocou`.

Após o término do segundo loop, verificamos se houve alguma troca através da variável `trocou`. Se não houve, significa que o array já está ordenado e podemos interromper o primeiro loop utilizando a instrução `break`.

Por fim, exibimos o array ordenado utilizando a função `print`.

Neste exemplo, consideramos um array de números como entrada, mas você pode adaptar o código para ordenar outros tipos de dados ou até mesmo personalizá-lo conforme suas necessidades.