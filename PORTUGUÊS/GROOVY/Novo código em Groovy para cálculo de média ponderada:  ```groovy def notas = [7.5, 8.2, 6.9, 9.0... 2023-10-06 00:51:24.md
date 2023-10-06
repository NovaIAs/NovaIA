Claro! Aqui está um código complexo em Groovy que envolve manipulação de strings, loops e estruturas condicionais:

```groovy
def texto = "Olá, seja bem-vindo ao meu código complexo em Groovy!"

// Função para inverter uma string
def inverterString(String texto) {
    def resultado = ""
    for (int i = texto.length() - 1; i >= 0; i--) {
        resultado += texto[i]
    }
    return resultado
}

// Função para contar o número de vogais em uma string
def contarVogais(String texto) {
    def vogais = ["a", "e", "i", "o", "u"]
    def contador = 0
    for (char letra : texto) {
        if (vogais.contains(letra.toLowerCase())) {
            contador++
        }
    }
    return contador
}

// Função para verificar se uma string é um palíndromo
def ehPalindromo(String texto) {
    def textoInvertido = inverterString(texto)
    return texto.toLowerCase() == textoInvertido.toLowerCase()
}

// Função para substituir todas as vogais por "*" em uma string
def substituirVogais(String texto) {
    def vogais = ["a", "e", "i", "o", "u"]
    for (char letra : texto) {
        if (vogais.contains(letra.toLowerCase())) {
            texto = texto.replace(letra, "*")
        }
    }
    return texto
}

// Imprime o texto original
println("Texto original: $texto")

// Inverte o texto
def textoInvertido = inverterString(texto)
println("Texto invertido: $textoInvertido")

// Conta o número de vogais no texto original
def numeroVogais = contarVogais(texto)
println("Número de vogais: $numeroVogais")

// Verifica se o texto original é um palíndromo
def palindromo = ehPalindromo(texto)
println("É palíndromo? $palindromo")

// Substitui as vogais por "*" no texto original
def textoSubstituido = substituirVogais(texto)
println("Texto com vogais substituídas: $textoSubstituido")
```

Neste código, criei quatro funções: `inverterString`, `contarVogais`, `ehPalindromo` e `substituirVogais`. A função `inverterString` recebe uma string como parâmetro e a inverte, retornando o resultado. A função `contarVogais` conta o número de vogais em uma string, ignorando maiúsculas e minúsculas. A função `ehPalindromo` verifica se uma string é um palíndromo, ou seja, se pode ser lida da mesma forma de trás para frente. A função `substituirVogais` substitui todas as vogais por "*" em uma string.

No final do código, há um exemplo de uso das funções, onde é mostrado o texto original, o texto invertido, o número de vogais no texto original, se o texto é um palíndromo e o texto com as vogais substituídas por "*".