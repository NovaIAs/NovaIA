Claro! Aqui está um exemplo de um código complexo em Swift que realiza a busca de um elemento em uma matriz multidimensional:

```swift
// Função para buscar um elemento em uma matriz multidimensional
func buscarElemento<T: Equatable>(elemento: T, em matriz: [[T]]) -> (Int, Int)? {
    for i in 0..<matriz.count {
        for j in 0..<matriz[i].count {
            if matriz[i][j] == elemento {
                return (i, j)
            }
        }
    }
    return nil
}

// Matriz de exemplo
let matriz = [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

// Elemento a ser buscado
let elemento = 5

// Chamada da função de busca
if let resultado = buscarElemento(elemento: elemento, em: matriz) {
    let (linha, coluna) = resultado
    print("O elemento \(elemento) foi encontrado na linha \(linha) e coluna \(coluna) da matriz.")
} else {
    print("O elemento \(elemento) não foi encontrado na matriz.")
}
```

Neste código, temos uma função chamada `buscarElemento` que recebe um elemento genérico `T` e uma matriz multidimensional `matriz`. A função utiliza um loop `for` aninhado para percorrer todos os elementos da matriz e verificar se algum deles é igual ao elemento buscado. Caso encontre o elemento, a função retorna uma tupla contendo a posição (linha e coluna) do elemento encontrado. Caso contrário, retorna `nil`.

Em seguida, é criada uma matriz de exemplo `matriz` e um elemento a ser buscado `elemento`. A função `buscarElemento` é chamada passando esses parâmetros e o resultado é armazenado em uma constante `resultado`. Se o resultado não for `nil`, ou seja, se o elemento foi encontrado na matriz, imprimimos a mensagem informando a posição do elemento na matriz. Caso contrário, imprimimos a mensagem informando que o elemento não foi encontrado.

Este código é apenas um exemplo complexo em Swift que demonstra a utilização de funções genéricas, matrizes multidimensionais e tuplas. É importante ressaltar que a complexidade do código pode variar de acordo com a aplicação e requisitos específicos.