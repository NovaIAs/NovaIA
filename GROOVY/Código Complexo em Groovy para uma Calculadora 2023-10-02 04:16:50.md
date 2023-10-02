Claro! Aqui está um exemplo de um código complexo em Groovy, que cria uma classe chamada "Calculadora" com métodos para realizar operações matemáticas básicas:

```groovy
class Calculadora {
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

def calculadora = new Calculadora()
println "Soma: " + calculadora.somar(5, 3)
println "Subtração: " + calculadora.subtrair(10, 7)
println "Multiplicação: " + calculadora.multiplicar(4, 6)
println "Divisão: " + calculadora.dividir(12, 3)
```

Neste código, definimos a classe `Calculadora` com quatro métodos: `somar`, `subtrair`, `multiplicar` e `dividir`. Cada método recebe dois parâmetros inteiros e retorna o resultado da operação.

No método `dividir`, também verificamos se o divisor é zero para evitar uma divisão por zero, lançando uma exceção `IllegalArgumentException` caso isso ocorra.

Em seguida, criamos uma instância da classe `Calculadora` e chamamos os métodos para realizar operações matemáticas simples, imprimindo os resultados na tela.

Espero que este código seja útil para você!