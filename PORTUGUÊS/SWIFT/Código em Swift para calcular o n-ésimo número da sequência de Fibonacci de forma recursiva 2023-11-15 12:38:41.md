Certamente! Aqui está um código complexo em Swift para calcular o n-ésimo número da sequência de Fibonacci de forma recursiva:

```swift
func fibonacci(_ n: Int) -> Int {
    if n <= 0 {
        return 0
    } else if n == 1 {
        return 1
    } else {
        return fibonacci(n - 1) + fibonacci(n - 2)
    }
}

guard let input = readLine(), let n = Int(input) else {
    print("Entrada inválida")
    exit(1)
}

if n < 0 {
    print("O valor de n deve ser não-negativo")
    exit(1)
}

let resultado = fibonacci(n)
print("O \(n)-ésimo número da sequência de Fibonacci é \(resultado)")
```

Este código cria uma função `fibonacci` que calcula e retorna o valor do n-ésimo número da sequência de Fibonacci. Para isso, ela utiliza uma abordagem recursiva.

A função `fibonacci` recebe um parâmetro `n` que representa a posição na sequência que desejamos calcular. Se `n` for menor ou igual a 0, o valor retornado será 0. Se `n` for igual a 1, o valor retornado será 1. Caso contrário, a função chama a si mesma passando `n - 1` e `n - 2`, somando os resultados.

Em seguida, o código lê uma entrada do usuário e converte-a em um número inteiro. Se a conversão falhar, uma mensagem de erro é exibida e o programa é encerrado. Caso contrário, o valor de entrada é verificado para garantir que seja não-negativo. Se for negativo, uma mensagem de erro é exibida e o programa é encerrado.

Por fim, o código chama a função `fibonacci` com o valor de entrada e imprime o resultado na saída, juntamente com uma mensagem identificando o n-ésimo número da sequência de Fibonacci correspondente.