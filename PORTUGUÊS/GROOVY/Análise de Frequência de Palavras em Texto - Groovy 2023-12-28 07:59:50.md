Claro! Aqui está um código em Groovy que irá realizar a análise de um texto, extraindo informações relevantes sobre as palavras utilizadas. 

```groovy
import java.util.regex.Pattern

def texto = """
Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nunc mattis vestibulum mauris, at consequat quam vestibulum ac. Duis maximus, risus quis cursus laoreet, arcu diam vulputate metus, et sagittis velit mi sit amet ligula. Nam non leo eget augue porttitor blandit vitae in elit. Donec consequat iaculis neque, in iaculis massa rutrum ac. Aenean ac viverra velit. Ut tincidunt lorem ut lacinia rutrum. Sed ultricies, mauris ut varius scelerisque, nunc lacus tincidunt lectus, non condimentum sapien tortor vitae tortor. Pellentesque maximus blandit lacus, vel tristique elit suscipit vel. Nulla facilisi. Mauris eget vestibulum ligula. Phasellus non porta leo, a pulvinar risus. Maecenas posuere tellus eu tellus tempus ultricies. Aliquam erat volutpat. Vivamus a aliquam velit.

Proin augue justo, aliquet at turpis ac, sollicitudin rutrum arcu. Fusce ullamcorper scelerisque eros at tristique. In sit amet nisi ante. Duis scelerisque tellus nec tortor maximus, id ullamcorper lectus iaculis. Nullam a condimentum ipsum. Maecenas vestibulum lacinia eros vel mollis. Cras auctor nulla at tortor luctus, nec condimentum augue lobortis. Suspendisse id viverra ex. Fusce mollis nunc nisi, at gravida mi fringilla a. Vestibulum ante ipsum primis in faucibus orci luctus et ultrices posuere cubilia curae; Phasellus congue massa vitae finibus congue. Sed vitae consequat massa. Fusce vitae ligula sed diam ornare tempor. Nam tincidunt, quam et facilisis lobortis, odio tortor gravida nisl, eget lobortis nisl tellus at velit. Fusce sit amet tellus et est fringilla condimentum. Cras ac metus sit amet sapien mattis ultrices.

Quisque consequat suscipit dictum. Sed sed velit ac elit luctus feugiat. Phasellus euismod semper sapien, at scelerisque lorem ultrices in. Phasellus a enim id felis dapibus fringilla. Nulla non ultricies mauris. Nullam id bibendum mauris, ut eleifend nulla. Nulla facilisi. Phasellus fermentum ullamcorper felis, at consequat sem rutrum non. Maecenas sit amet fringilla odio. Nunc vel arcu auctor, consectetur quam sed, euismod sapien. Aenean vehicula, eros nec posuere eleifend, nulla purus hendrerit eros, nec laoreet orci risus tincidunt justo. Sed luctus erat ut luctus porttitor. Sed fringilla leo in arcu varius, a semper justo rutrum. Phasellus euismod, arcu ut faucibus malesuada, mauris mi luctus orci, et tempor sem tortor nec lacus.
"""

def contarPalavras(texto) {
    def padrao = Pattern.compile("\\b\\w+\\b")
    def matcher = padrao.matcher(texto)
    def palavras = [:]
    
    while (matcher.find()) {
        def palavra = matcher.group().toLowerCase()
        palavras[palavra] = palavras.containsKey(palavra) ? palavras[palavra] + 1 : 1
    }
    
    palavras = palavras.sort { a, b -> b.value <=> a.value }
    palavras.each { palavra, frequencia ->
        println("$palavra: $frequencia")
    }
}

contarPalavras(texto)
```

Este código recebe um texto como entrada e realiza a contagem de palavras presentes nesse texto. Primeiro, é criado um padrão de busca utilizando uma expressão regular que identifica palavras (\b\w+\b). Em seguida, é criado um objeto Matcher para procurar todas as ocorrências desse padrão no texto.

A cada combinação encontrada, a palavra é convertida para minúsculo e é verificado se ela já existe no dicionário `palavras`. Caso já exista, é incrementado o contador. Caso contrário, é adicionada no dicionário com contador igual a 1.

No final, o dicionário `palavras` é classificado em ordem decrescente de frequência e cada palavra é impressa juntamente com a sua frequência.

No código de exemplo fornecido, o texto utilizado foi um parágrafo "Lorem ipsum" fictício apenas para demonstrar o funcionamento do código. Você pode substituir o texto pela sua própria entrada para obter a contagem de palavras correspondente.