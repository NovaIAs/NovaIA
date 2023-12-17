Claro! Aqui está um código complexo em Groovy:

```groovy
import java.util.Random

def numerosPrimos = []

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

// Função para gerar uma lista de números primos
def gerarNumerosPrimos(quantidade) {
    def random = new Random()

    while (numerosPrimos.size() < quantidade) {
        def numero = random.nextInt(1000) + 1

        if (ehPrimo(numero) && !numerosPrimos.contains(numero)) {
            numerosPrimos.add(numero)
        }
    }
}

// Função para calcular a soma dos números primos gerados
def calcularSomaPrimos() {
    def soma = 0

    numerosPrimos.each { numero ->
        soma += numero
    }

    return soma
}

// Função para imprimir os números primos gerados
def imprimirNumerosPrimos() {
    numerosPrimos.each { numero ->
        println(numero)
    }
}

gerarNumerosPrimos(10)
imprimirNumerosPrimos()

def somaPrimos = calcularSomaPrimos()
println("A soma dos números primos gerados é: " + somaPrimos)
```

Neste código em Groovy, eu criei um programa que gera uma lista de números primos aleatórios. 

A função `ehPrimo(numero)` verifica se um número é primo. Ela recebe um número como parâmetro e retorna `true` se for primo ou `false` caso contrário. 

A função `gerarNumerosPrimos(quantidade)` gera uma lista de números primos. Ela recebe a quantidade de números primos que devem ser gerados como parâmetro. Utilizando um loop `while`, o programa gera números aleatórios entre 1 e 1000 até atingir a quantidade desejada. Se o número gerado for primo e ainda não estiver na lista `numerosPrimos`, ele é adicionado à lista.

A função `calcularSomaPrimos()` calcula a soma dos números primos gerados. Ela percorre a lista `numerosPrimos` utilizando um loop `each` e vai somando os valores.

A função `imprimirNumerosPrimos()` imprime os números primos gerados, percorrendo a lista `numerosPrimos` utilizando um loop `each` e utilizando o comando `println(numero)` para imprimir cada número.

No final do código, é chamada a função `gerarNumerosPrimos(10)` para gerar 10 números primos, em seguida é chamada a função `imprimirNumerosPrimos()` para imprimir os números gerados e, por fim, a função `calcularSomaPrimos()` é chamada para calcular a soma dos números primos e é impresso o resultado.