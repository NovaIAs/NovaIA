Claro! Aqui está um código complexo em Groovy que envolve manipulação de strings, estruturas de repetição e chamadas a métodos:

```groovy
def texto = "NovaIA é incrível!"

def contarVogais(texto) {
    def vogais = ['a', 'e', 'i', 'o', 'u']
    def contador = 0
    
    for (char letra : texto.toCharArray()) {
        if (vogais.contains(letra.toLowerCase())) {
            contador++
        }
    }
    
    return contador
}

def inverterTexto(texto) {
    def textoInvertido = ""
    
    for (int i = texto.length() - 1; i >= 0; i--) {
        textoInvertido += texto[i]
    }
    
    return textoInvertido
}

def contarPalavras(texto) {
    def palavras = texto.split("\\s+")
    return palavras.size()
}

def textoInvertido = inverterTexto(texto)
def totalVogais = contarVogais(texto)
def totalPalavras = contarPalavras(texto)

println "Texto Original: $texto"
println "Texto Invertido: $textoInvertido"
println "Total de Vogais: $totalVogais"
println "Total de Palavras: $totalPalavras"
```

Nesse código, temos três funções principais: `contarVogais`, `inverterTexto` e `contarPalavras`.

A função `contarVogais` recebe uma string como parâmetro, inicializa um array com as vogais e um contador. Em seguida, ela percorre cada caractere da string usando um loop for-each. Se o caractere for uma vogal (considerando tanto minúsculas quanto maiúsculas), o contador é incrementado. No final, o valor do contador é retornado.

A função `inverterTexto` também recebe uma string como parâmetro. Ela inicializa uma nova string vazia e, em seguida, percorre a string original de trás para frente usando um loop for com uma variável de controle `i`. A cada iteração, o caractere correspondente é adicionado à string invertida. No final, a string invertida é retornada.

A função `contarPalavras` recebe uma string como parâmetro. Ela utiliza o método `split` para separar a string em palavras, considerando qualquer sequência de espaços em branco como delimitador. O resultado é um array de palavras, e o tamanho desse array é retornado como total de palavras.

No programa principal, o texto original é definido como "NovaIA é incrível!". Em seguida, é chamada a função `inverterTexto` para obter o texto invertido. Também são chamadas as funções `contarVogais` e `contarPalavras` para obter o total de vogais e palavras do texto original.

Por fim, os resultados são exibidos no console usando a função `println`.

Esse código realiza algumas operações interessantes e demonstra diferentes conceitos de programação em Groovy.