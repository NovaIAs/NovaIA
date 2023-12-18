Claro! Aqui está um código complexo em Swift que envolve manipulação de dados, estruturas de controle e funções:

```swift
// Função para verificar se um número é primo
func isPrime(_ number: Int) -> Bool {
    if number <= 1 {
        return false
    }
    
    for i in 2..<number {
        if number % i == 0 {
            return false
        }
    }
    
    return true
}

// Função para calcular o fatorial de um número
func factorial(_ number: Int) -> Int {
    var result = 1
    
    if number == 0 {
        return result
    }
    
    for i in 1...number {
        result *= i
    }
    
    return result
}

// Função para inverter uma string
func reverseString(_ string: String) -> String {
    var reversedString = ""
    
    for character in string {
        reversedString = String(character) + reversedString
    }
    
    return reversedString
}

// Função principal
func main() {
    let number = 17
    
    if isPrime(number) {
        print("\(number) é um número primo!")
    } else {
        print("\(number) não é um número primo!")
    }
    
    let factorialNumber = 5
    let factorialResult = factorial(factorialNumber)
    print("O fatorial de \(factorialNumber) é \(factorialResult)")
    
    let stringToReverse = "Olá, mundo!"
    let reversedString = reverseString(stringToReverse)
    print("A string reversa de '\(stringToReverse)' é '\(reversedString)'")
}

// Chamada da função principal
main()
```

Neste código, temos três funções principais: `isPrime` para verificar se um número é primo, `factorial` para calcular o fatorial de um número e `reverseString` para inverter uma string.

Na função `main`, são feitas chamadas dessas funções para demonstrar seu uso. O número 17 é verificado se é primo, o fatorial de 5 é calculado e uma string é invertida. Os resultados são impressos no console.

Espero que este código atenda às suas necessidades!