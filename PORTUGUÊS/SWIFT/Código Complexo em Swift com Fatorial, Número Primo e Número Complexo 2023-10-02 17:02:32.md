Certamente! Aqui está um código complexo escrito em Swift:

```swift
import Foundation

// Definindo uma função para calcular o fatorial de um número
func calcularFatorial(_ n: Int) -> Int {
    if n == 0 {
        return 1
    } else {
        return n * calcularFatorial(n - 1)
    }
}

// Função para verificar se um número é primo
func verificarPrimo(_ n: Int) -> Bool {
    if n <= 1 {
        return false
    }
    
    for i in 2..<n {
        if n % i == 0 {
            return false
        }
    }
    
    return true
}

// Classe para representar um número complexo
class NumeroComplexo {
    var parteReal: Double
    var parteImaginaria: Double
    
    init(parteReal: Double, parteImaginaria: Double) {
        self.parteReal = parteReal
        self.parteImaginaria = parteImaginaria
    }
    
    func somar(_ numero: NumeroComplexo) -> NumeroComplexo {
        let real = parteReal + numero.parteReal
        let imaginaria = parteImaginaria + numero.parteImaginaria
        
        return NumeroComplexo(parteReal: real, parteImaginaria: imaginaria)
    }
    
    func multiplicar(_ numero: NumeroComplexo) -> NumeroComplexo {
        let real = parteReal * numero.parteReal - parteImaginaria * numero.parteImaginaria
        let imaginaria = parteReal * numero.parteImaginaria + parteImaginaria * numero.parteReal
        
        return NumeroComplexo(parteReal: real, parteImaginaria: imaginaria)
    }
}

// Exemplo de uso das funções e da classe
let numero = 5
let fatorial = calcularFatorial(numero)
print("O fatorial de \(numero) é \(fatorial)")

let numeroPrimo = 17
let isPrimo = verificarPrimo(numeroPrimo)
if isPrimo {
    print("\(numeroPrimo) é um número primo")
} else {
    print("\(numeroPrimo) não é um número primo")
}

let complexo1 = NumeroComplexo(parteReal: 2, parteImaginaria: 3)
let complexo2 = NumeroComplexo(parteReal: -1, parteImaginaria: 4)
let soma = complexo1.somar(complexo2)
print("A soma dos números complexos é \(soma.parteReal) + \(soma.parteImaginaria)i")

let multiplicacao = complexo1.multiplicar(complexo2)
print("A multiplicação dos números complexos é \(multiplicacao.parteReal) + \(multiplicacao.parteImaginaria)i")
```

Esse código apresenta uma função para calcular o fatorial de um número, uma função para verificar se um número é primo e uma classe para representar números complexos, com métodos para realizar a adição e multiplicação desses números.