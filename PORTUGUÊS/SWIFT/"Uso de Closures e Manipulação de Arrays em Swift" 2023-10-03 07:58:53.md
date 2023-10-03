Claro! Aqui está um código complexo em Swift que envolve o uso de closures e manipulação de arrays:

```swift
// Criação de um array de números inteiros
var numbers = [1, 2, 3, 4, 5]

// Definição de uma closure que recebe um número inteiro e retorna seu quadrado
let square: (Int) -> Int = { number in
    return number * number
}

// Mapeamento dos elementos do array usando a closure square
let squaredNumbers = numbers.map(square)

// Impressão dos números originais e seus respectivos quadrados
for (index, number) in numbers.enumerated() {
    print("O número \(number) ao quadrado é igual a \(squaredNumbers[index])")
}
```

Explicação do código:

1. Primeiro, criamos um array de números inteiros chamado `numbers` contendo os números de 1 a 5.

2. Em seguida, definimos uma closure chamada `square` que recebe um número inteiro como parâmetro e retorna o quadrado desse número. A closure utiliza a sintaxe `{ number in }` para definir o bloco de código.

3. Utilizamos a função `map` do array `numbers` para criar um novo array chamado `squaredNumbers`, onde cada elemento é o quadrado do elemento correspondente em `numbers`. A função `map` aplica a closure `square` em cada elemento do array original.

4. Por fim, percorremos os números originais e seus respectivos quadrados usando um loop `for-in` e imprimimos a mensagem correspondente para cada par de números.

Esse código demonstra o uso de closures e a manipulação de arrays em Swift. A closure `square` é uma função de ordem superior que pode ser passada como argumento para outras funções, como a função `map`. Isso permite uma maior flexibilidade e reutilização de código.