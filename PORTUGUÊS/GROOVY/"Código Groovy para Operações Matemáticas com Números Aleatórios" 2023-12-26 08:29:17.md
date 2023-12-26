Claro! Aqui está um código complexo em Groovy que realiza uma série de operações matemáticas com números aleatórios. Ele consiste em uma função principal chamada "operacoesMatematicas", que é responsável por chamar várias outras funções para realizar operações específicas. Vou explicar o código linha por linha:

```groovy
import java.util.Random

def operacoesMatematicas() {
    def random = new Random()

    def numero1 = random.nextInt(100)
    def numero2 = random.nextInt(100)

    def soma = somarNumeros(numero1, numero2)
    def subtracao = subtrairNumeros(numero1, numero2)
    def multiplicacao = multiplicarNumeros(numero1, numero2)
    def divisao = dividirNumeros(numero1, numero2)

    println "Números gerados: $numero1 e $numero2"
    println "Soma: $soma"
    println "Subtração: $subtracao"
    println "Multiplicação: $multiplicacao"
    println "Divisão: $divisao"
}

def somarNumeros(numero1, numero2) {
    return numero1 + numero2
}

def subtrairNumeros(numero1, numero2) {
    return numero1 - numero2
}

def multiplicarNumeros(numero1, numero2) {
    return numero1 * numero2
}

def dividirNumeros(numero1, numero2) {
    if (numero2 != 0) {
        return numero1 / numero2
    } else {
        println "Impossível dividir por zero!"
        return null
    }
}

operacoesMatematicas()
```

Explicação do código:
1. Importamos a classe `Random` do pacote `java.util`, que nos permitirá gerar números aleatórios.
2. Definimos a função `operacoesMatematicas`, que será a função principal do código.
3. Criamos uma instância da classe `Random` chamada `random`.
4. Geramos dois números aleatórios entre 0 e 100 e os armazenamos nas variáveis `numero1` e `numero2`.
5. Chamamos as outras funções para realizar as operações matemáticas de soma, subtração, multiplicação e divisão, passando os números gerados como argumentos.
6. Imprimimos na tela os números gerados e os resultados das operações matemáticas.
7. Definimos as funções `somarNumeros`, `subtrairNumeros`, `multiplicarNumeros` e `dividirNumeros`, que recebem dois números como parâmetros e retornam o resultado da operação correspondente.
8. Na função `dividirNumeros`, verificamos se o segundo número é diferente de zero antes de realizar a divisão, para evitar a divisão por zero.
9. Se o segundo número for zero, exibimos uma mensagem de erro na tela e retornamos `null`.
10. Por fim, chamamos a função `operacoesMatematicas` para iniciar a execução do código.