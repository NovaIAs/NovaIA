Claro! Aqui está um exemplo de um código complexo em Groovy que utiliza várias funcionalidades da linguagem:

```groovy
// Função para verificar se um número é primo
def ehPrimo(numero) {
    if (numero <= 1) {
        return false
    }
    
    for (int i = 2; i <= Math.sqrt(numero); i++) {
        if (numero % i == 0) {
            return false
        }
    }
    
    return true
}

// Função para calcular o fatorial de um número
def fatorial(numero) {
    def resultado = 1
    
    for (int i = 2; i <= numero; i++) {
        resultado *= i
    }
    
    return resultado
}

// Função para calcular a sequência de Fibonacci até um determinado número
def fibonacci(quantidade) {
    def sequencia = [0, 1]
    
    for (int i = 2; i < quantidade; i++) {
        sequencia << sequencia[i - 1] + sequencia[i - 2]
    }
    
    return sequencia
}

// Função principal
def main() {
    println("Bem-vindo ao programa complexo em Groovy!")
    
    def numero = 17
    
    if (ehPrimo(numero)) {
        println("${numero} é um número primo!")
    } else {
        println("${numero} não é um número primo!")
    }
    
    def fatorialNumero = fatorial(numero)
    println("O fatorial de ${numero} é ${fatorialNumero}")
    
    def sequenciaFibonacci = fibonacci(numero)
    println("A sequência de Fibonacci até ${numero} é: ${sequenciaFibonacci}")
}

// Chamada da função principal
main()
```

Neste código, temos três funções principais: `ehPrimo`, que verifica se um número é primo; `fatorial`, que calcula o fatorial de um número; e `fibonacci`, que gera a sequência de Fibonacci até um determinado número.

Além disso, temos a função `main`, que é a função principal do programa. Nela, é feito um exemplo de uso das três funções, onde é verificado se um número é primo, calculado o fatorial desse número e gerada a sequência de Fibonacci até esse número.

Ao executar esse código em Groovy, ele irá imprimir as informações sobre o número fornecido, indicando se ele é primo ou não, calculando seu fatorial e exibindo a sequência de Fibonacci até ele.