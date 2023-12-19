Claro! Aqui está um código complexo em Groovy:

```groovy
import groovy.transform.CompileStatic

@CompileStatic
class Calculadora {
    static int somar(int a, int b) {
        return a + b
    }

    static int subtrair(int a, int b) {
        return a - b
    }

    static int multiplicar(int a, int b) {
        return a * b
    }

    static double dividir(int a, int b) {
        if (b == 0) {
            throw new ArithmeticException("Divisão por zero não é permitida.")
        }
        return a / b
    }
}

class Main {
    static void main(String[] args) {
        try {
            int resultadoSoma = Calculadora.somar(10, 5)
            println("Resultado da soma: " + resultadoSoma)

            int resultadoSubtracao = Calculadora.subtrair(10, 5)
            println("Resultado da subtração: " + resultadoSubtracao)

            int resultadoMultiplicacao = Calculadora.multiplicar(10, 5)
            println("Resultado da multiplicação: " + resultadoMultiplicacao)

            double resultadoDivisao = Calculadora.dividir(10, 5)
            println("Resultado da divisão: " + resultadoDivisao)
        } catch (ArithmeticException e) {
            println("Erro: " + e.message)
        }
    }
}
```

Neste código, temos uma classe `Calculadora` com métodos estáticos para realizar operações matemáticas básicas: soma, subtração, multiplicação e divisão. A classe `Calculadora` é anotada com `@CompileStatic` para obter maior eficiência e detecção de erros em tempo de compilação.

Na classe `Main`, dentro do método `main`, utilizamos a classe `Calculadora` para realizar algumas operações e imprimir os resultados na tela. Também é tratada a exceção `ArithmeticException` caso ocorra uma divisão por zero.

Esse código exemplifica o uso de classes, métodos estáticos, tratamento de exceções, e demonstra algumas boas práticas de programação em Groovy.