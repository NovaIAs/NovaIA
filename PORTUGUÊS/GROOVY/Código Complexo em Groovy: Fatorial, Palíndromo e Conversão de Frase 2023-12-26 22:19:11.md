Claro! Aqui está um exemplo de código complexo em Groovy que realiza uma série de operações matemáticas e manipulações de string:

```groovy
// Função para calcular o fatorial de um número
def fatorial(n) {
    if (n == 0 || n == 1) {
        return 1
    } else {
        return n * fatorial(n - 1)
    }
}

// Função para verificar se uma palavra é um palíndromo
def ehPalindromo(palavra) {
    def palavraInvertida = palavra.reverse()
    return palavra == palavraInvertida
}

// Função para converter uma frase em um array de palavras
def converterFraseEmArray(frase) {
    return frase.split(" ")
}

// Função principal
def main() {
    def numero = 5
    println("O fatorial de ${numero} é ${fatorial(numero)}")
    
    def palavra = "arara"
    if (ehPalindromo(palavra)) {
        println("${palavra} é um palíndromo")
    } else {
        println("${palavra} não é um palíndromo")
    }
    
    def frase = "Esta é uma frase de exemplo"
    def palavras = converterFraseEmArray(frase)
    println("A frase possui ${palavras.size()} palavras")
}

// Chamada da função principal
main()
```

Neste código, temos três funções principais: `fatorial`, `ehPalindromo` e `converterFraseEmArray`. 

A função `fatorial` calcula o fatorial de um número usando uma recursão simples. Ela verifica se o número é igual a 0 ou 1 e retorna 1 nesses casos. Caso contrário, ela chama a si mesma com o argumento decrementado em 1 e multiplica o número pelo resultado dessa chamada recursiva.

A função `ehPalindromo` verifica se uma palavra é um palíndromo. Ela inverte a palavra original usando o método `reverse` e compara com a palavra original. Se as duas palavras forem iguais, significa que a palavra é um palíndromo.

A função `converterFraseEmArray` recebe uma frase como argumento e a divide em um array de palavras utilizando o método `split` e o espaço em branco como delimitador.

A função `main` é a função principal do programa. Ela define uma variável `numero` com o valor 5 e calcula o fatorial desse número utilizando a função `fatorial`. Em seguida, ela verifica se a palavra "arara" é um palíndromo utilizando a função `ehPalindromo`. Por fim, ela converte a frase "Esta é uma frase de exemplo" em um array de palavras utilizando a função `converterFraseEmArray` e imprime o tamanho desse array.

O código finaliza com a chamada da função `main`.