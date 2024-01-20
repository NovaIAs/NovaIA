```swift
// Función para calcular el factorial de un número
func factorial(_ n: Int) -> UInt {
    if n == 0 {
        return 1
    } else {
        return UInt(n) * factorial(n-1)
    }
}

// Función para calcular el número de combinaciones de n elementos tomados de k en k
func combinaciones(_ n: Int, _ k: Int) -> UInt {
    return factorial(n) / (factorial(k) * factorial(n-k))
}

// Función para calcular el número de permutaciones de n elementos tomados de k en k
func permutaciones(_ n: Int, _ k: Int) -> UInt {
    return factorial(n) / factorial(n-k)
}

// Función para calcular el máximo común divisor de dos números
func mcd(_ a: Int, _ b: Int) -> Int {
    if b == 0 {
        return a
    } else {
        return mcd(b, a % b)
    }
}

// Función para calcular el mínimo común múltiplo de dos números
func mcm(_ a: Int, _ b: Int) -> Int {
    return (a * b) / mcd(a, b)
}

// Función para calcular el inverso multiplicativo de un número en un módulo
func inverso(_ a: Int, _ m: Int) -> Int {
    var x0 = 0, x1 = 1, y0 = 1, y1 = 0
    while a > 1 {
        let q = a / m
        let t = m
        m = a % m
        a = t
        let x2 = x0 - q * x1
        let y2 = y0 - q * y1
        x0 = x1
        x1 = x2
        y0 = y1
        y1 = y2
    }
    if x1 < 0 {
        x1 += m
    }
    return x1
}

// Función para calcular la matriz adjunta de una matriz
func adjunta(_ A: [[Int]]) -> [[Int]] {
    var B = [[Int]]()
    for i in 0..<A.count {
        var row = [Int]()
        for j in 0..<A[0].count {
            var submatrix = [[Int]]()
            for k in 0..<A.count {
                if k != i {
                    submatrix.append(Array(A[k][0..<j] + A[k][(j+1)...]))
                }
            }
            row.append(Int(pow(Double(-1), Double(i+j))) * mcd(submatrix))
        }
        B.append(row)
    }
    return B
}

// Función para calcular la matriz inversa de una matriz
func inversa(_ A: [[Int]]) -> [[Int]] {
    let detA = mcd(A)
    if detA == 0 {
        return [[Int]]()
    } else {
        let adjA = adjunta(A)
        var B = [[Int]]()
        for i in 0..<A.count {
            var row = [Int]()
            for j in 0..<A[0].count {
                row.append((adjA[j][i] * inverso(detA, A.count)) % A.count)
            }
            B.append(row)
        }
        return B