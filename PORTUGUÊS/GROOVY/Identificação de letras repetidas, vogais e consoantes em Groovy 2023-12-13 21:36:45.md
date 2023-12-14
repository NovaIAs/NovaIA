Certamente! Aqui está um código complexo em Groovy que envolve várias funcionalidades e é altamente diferenciado:

```groovy
import java.util.regex.Pattern

def palavra = "programação"
def letrasRepetidas = getLetrasRepetidas(palavra)
def vogais = getVogais(palavra)
def consoantes = getConsoantes(palavra)

println "Palavra: $palavra"
println "Letras repetidas: $letrasRepetidas"
println "Vogais: $vogais"
println "Consoantes: $consoantes"

def getLetrasRepetidas(palavra) {
    def letrasRepetidas = []
    def letras = palavra.toList()
    def count = letras.groupBy().collect { key, value -> [key, value.size()] }
    count.each {
        if (it[1] > 1) {
            letrasRepetidas.add(it[0])
        }
    }
    letrasRepetidas
}

def getVogais(palavra) {
    def vogais = []
    def letras = palavra.toList()
    letras.each {
        if (Pattern.matches("[aeiou]", it.toString())) {
            vogais.add(it)
        }
    }
    vogais
}

def getConsoantes(palavra) {
    def consoantes = []
    def letras = palavra.toList()
    letras.each {
        if (Pattern.matches("[bcdfghjklmnpqrstvwxyz]", it.toString())) {
            consoantes.add(it)
        }
    }
    consoantes
}
```

Explicação do código:

1. Começamos importando a classe `Pattern` do pacote `java.util.regex`, que nos permitirá fazer correspondência de padrões.
2. Em seguida, definimos a variável `palavra` com a palavra "programação" como exemplo.
3. Chamamos três funções: `getLetrasRepetidas`, `getVogais` e `getConsoantes` para obter informações sobre a palavra.
4. A função `getLetrasRepetidas` recebe a palavra como parâmetro e retorna uma lista contendo as letras repetidas na palavra.
5. Dentro dessa função, convertemos a palavra em uma lista de caracteres e contamos quantas vezes cada letra aparece usando `groupBy` e `collect`.
6. Percorremos o resultado e adicionamos as letras repetidas à lista `letrasRepetidas`.
7. A função `getVogais` recebe a palavra como parâmetro e retorna uma lista contendo as vogais presentes na palavra.
8. Dentro dessa função, convertemos a palavra em uma lista de caracteres e verificamos, usando uma expressão regular, se cada caractere é uma vogal.
9. Se for uma vogal, adicionamos à lista `vogais`.
10. A função `getConsoantes` recebe a palavra como parâmetro e retorna uma lista contendo as consoantes presentes na palavra.
11. Dentro dessa função, convertemos a palavra em uma lista de caracteres e verificamos, usando uma expressão regular, se cada caractere é uma consoante.
12. Se for uma consoante, adicionamos à lista `consoantes`.
13. Por fim, imprimimos as informações obtidas: a palavra original, as letras repetidas, as vogais e as consoantes.

Este código mostra um exemplo de como manipular uma palavra em Groovy, identificar letras repetidas, vogais e consoantes, e exibir essas informações de forma clara e legível.