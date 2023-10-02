Claro! Aqui está um código complexo em Swift que cria uma matriz mágica de tamanho variável:

```swift
func createMagicSquare(ofSize size: Int) -> [[Int]] {
    var magicSquare = Array(repeating: Array(repeating: 0, count: size), count: size)
    
    var row = 0
    var col = size / 2
    magicSquare[row][col] = 1
    
    for num in 2...(size*size) {
        let prevRow = row
        let prevCol = col
        
        row -= 1
        col += 1
        
        if row < 0 {
            row = size - 1
        }
        
        if col == size {
            col = 0
        }
        
        if magicSquare[row][col] != 0 {
            row = prevRow + 1
            col = prevCol
            
            if row == size {
                row = 0
            }
        }
        
        magicSquare[row][col] = num
    }
    
    return magicSquare
}

let size = 5 // Tamanho da matriz mágica, pode ser alterado
let magicSquare = createMagicSquare(ofSize: size)

for row in magicSquare {
    print(row)
}
```

Este código cria uma matriz mágica de tamanho `size` (definido por você) e a preenche com números de 1 a `size*size`. Uma matriz mágica é uma matriz quadrada em que a soma de cada linha, coluna e diagonal principal é a mesma.