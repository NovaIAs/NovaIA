Claro! Aqui está um código complexo em Groovy:

```groovy
import groovy.transform.CompileStatic

@CompileStatic
class Calculadora {
    static def soma(int a, int b) {
        def resultado = a + b
        println("A soma de $a com $b é igual a $resultado")
        return resultado
    }

    static def subtracao(int a, int b) {
        def resultado = a - b
        println("A subtração de $a por $b é igual a $resultado")
        return resultado
    }

    static def multiplicacao(int a, int b) {
        def resultado = a * b
        println("A multiplicação de $a por $b é igual a $resultado")
        return resultado
    }

    static def divisao(int a, int b) {
        if (b == 0) {
            throw new ArithmeticException("Divisão por zero não é permitida")
        }
        def resultado = a / b
        println("A divisão de $a por $b é igual a $resultado")
        return resultado
    }
}

def calcular() {
    def a = 10
    def b = 5

    Calculadora.soma(a, b)
    Calculadora.subtracao(a, b)
    Calculadora.multiplicacao(a, b)
    Calculadora.divisao(a, b)
}

calcular()
```

Neste código, estamos criando uma classe chamada `Calculadora` que possui métodos estáticos para realizar operações matemáticas básicas. 

Utilizamos a anotação `@CompileStatic` para tornar o código mais eficiente e evitar erros em tempo de execução.

Dentro da classe, temos os métodos `soma`, `subtracao`, `multiplicacao` e `divisao`, que recebem dois números inteiros como parâmetros e realizam as operações correspondentes. Os resultados são impressos na tela e retornados como valor de retorno do método.

No método `divisao`, adicionamos uma verificação para evitar a divisão por zero. Se o segundo número for igual a zero, lançamos uma exceção `ArithmeticException` informando que a divisão por zero não é permitida.

Fora da classe, definimos um método `calcular` que realiza uma série de operações utilizando a classe `Calculadora`. Neste exemplo, estamos utilizando os números 10 e 5, mas você pode alterar esses valores conforme necessário.

Por fim, chamamos o método `calcular` para executar o código e ver os resultados das operações na saída.