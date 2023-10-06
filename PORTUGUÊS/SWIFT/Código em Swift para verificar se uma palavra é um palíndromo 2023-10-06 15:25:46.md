Certamente! Aqui está um código complexo em Swift que envolve manipulação de strings, loops e estruturas condicionais. O código a seguir é um exemplo de um programa que verifica se uma palavra é um palíndromo.

```swift
func isPalindrome(word: String) -> Bool {
    let cleanWord = word.lowercased().filter { $0.isLetter }
    let reversedWord = String(cleanWord.reversed())
    
    return cleanWord == reversedWord
}

func checkPalindrome() {
    print("Digite uma palavra:")
    if let input = readLine() {
        if isPalindrome(word: input) {
            print("\(input) é um palíndromo!")
        } else {
            print("\(input) não é um palíndromo.")
        }
    }
}

checkPalindrome()
```

Este código começa com a definição da função `isPalindrome`, que recebe uma palavra como parâmetro e retorna um valor booleano indicando se a palavra é um palíndromo ou não. 

Dentro dessa função, primeiro usamos o método `lowercased()` para converter a palavra para minúsculas, garantindo que não haja diferença entre letras maiúsculas e minúsculas ao verificar se é um palíndromo. Em seguida, usamos o método `filter` em combinação com a propriedade `isLetter` para remover todos os caracteres não alfabéticos da palavra.

A variável `cleanWord` recebe a palavra filtrada e, em seguida, usamos o método `reversed()` para obter a versão invertida da palavra. Essa versão invertida é armazenada na variável `reversedWord`.

Por fim, comparamos a palavra original (`cleanWord`) com a versão invertida (`reversedWord`) e retornamos `true` se forem iguais, indicando que a palavra é um palíndromo, ou `false` caso contrário.

A segunda função, `checkPalindrome`, é responsável por solicitar ao usuário que digite uma palavra e, em seguida, chama a função `isPalindrome` para verificar se a palavra é um palíndromo ou não. Em seguida, imprime a mensagem correspondente na saída.

Para testar esse código, você pode executá-lo em um playground do Swift ou em um ambiente de desenvolvimento Swift.