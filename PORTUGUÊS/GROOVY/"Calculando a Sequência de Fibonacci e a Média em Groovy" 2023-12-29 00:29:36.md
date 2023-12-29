Claro! Aqui está um código complexo em Groovy:

```groovy
def fibonacci = { n ->
    if (n <= 0) {
        return []
    } else if (n == 1) {
        return [0]
    } else if (n == 2) {
        return [0, 1]
    } else {
        def sequence = [0, 1]
        (3..n).each {
            sequence << sequence[-1] + sequence[-2]
        }
        return sequence
    }
}

def calcularMedia = { lista ->
    def soma = lista.sum()
    return soma / lista.size()
}

def imprimirResultado = { listaFibonacci, media ->
    println("Sequência de Fibonacci: ${listaFibonacci}")
    println("Média da sequência: ${media}")
}

def n = 10
def listaFibonacci = fibonacci(n)
def media = calcularMedia(listaFibonacci)
imprimirResultado(listaFibonacci, media)
```

Neste código, temos três funções definidas:

1. `fibonacci`: Esta função recebe um número `n` como parâmetro e retorna uma lista com os `n` primeiros números da sequência de Fibonacci. Utiliza uma abordagem iterativa para calcular a sequência.

2. `calcularMedia`: Esta função recebe uma lista como parâmetro e retorna a média dos valores presentes na lista. Utiliza o método `sum()` do Groovy para calcular a soma dos elementos da lista e, em seguida, divide pelo tamanho da lista.

3. `imprimirResultado`: Esta função recebe a lista de Fibonacci e a média como parâmetros e imprime o resultado na saída padrão. Utiliza a função `println()` para exibir a sequência de Fibonacci e a média.

Na parte final do código, definimos uma variável `n` com o valor 10, chamamos a função `fibonacci()` passando `n` como argumento para obter a lista de Fibonacci para os primeiros 10 números e, em seguida, calculamos a média dessa lista usando a função `calcularMedia()`. Por fim, chamamos a função `imprimirResultado()` para exibir os resultados na tela.