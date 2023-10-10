Claro! Aqui está um código complexo em Groovy:

```groovy
import java.util.Random

class GeradorAleatorio {
    def gerarNumeroAleatorio(int min, int max) {
        Random random = new Random()
        return random.nextInt((max - min) + 1) + min
    }
}

class CalculadoraMatematica {
    def somar(int a, int b) {
        return a + b
    }

    def subtrair(int a, int b) {
        return a - b
    }

    def multiplicar(int a, int b) {
        return a * b
    }

    def dividir(int a, int b) {
        if (b == 0) {
            throw new IllegalArgumentException("Não é possível dividir por zero!")
        }
        return a / b
    }
}

def gerador = new GeradorAleatorio()
def calculadora = new CalculadoraMatematica()

try {
    def numero1 = gerador.gerarNumeroAleatorio(1, 10)
    def numero2 = gerador.gerarNumeroAleatorio(1, 10)

    println("Números gerados: $numero1 e $numero2")

    def soma = calculadora.somar(numero1, numero2)
    def subtracao = calculadora.subtrair(numero1, numero2)
    def multiplicacao = calculadora.multiplicar(numero1, numero2)
    def divisao = calculadora.dividir(numero1, numero2)

    println("Resultado da soma: $soma")
    println("Resultado da subtração: $subtracao")
    println("Resultado da multiplicação: $multiplicacao")
    println("Resultado da divisão: $divisao")
} catch (IllegalArgumentException e) {
    println(e.message)
}
```

Este código em Groovy inclui duas classes: `GeradorAleatorio` e `CalculadoraMatematica`.

A classe `GeradorAleatorio` possui um método `gerarNumeroAleatorio` que utiliza a classe `Random` para gerar um número inteiro aleatório entre os valores mínimos (`min`) e máximos (`max`) fornecidos como argumentos.

A classe `CalculadoraMatematica` possui quatro métodos: `somar`, `subtrair`, `multiplicar` e `dividir`. Cada um desses métodos realiza a operação matemática correspondente com os dois números fornecidos como argumentos.

No bloco principal do programa, criamos uma instância do `GeradorAleatorio` e do `CalculadoraMatematica`. Em seguida, geramos dois números aleatórios, realizamos as operações matemáticas utilizando a calculadora e imprimimos os resultados.

Se ocorrer uma tentativa de divisão por zero, o código irá lançar uma exceção do tipo `IllegalArgumentException` e imprimir a mensagem "Não é possível dividir por zero!".

Espero que este código complexo e diferenciado em Groovy atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais ajuda, sinta-se à vontade para perguntar.