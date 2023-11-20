Claro! Aqui está um código complexo em Groovy que busca e exibe informações sobre os filmes de uma plataforma de streaming:

```groovy
import groovy.json.JsonSlurper

def respostaAPI = buscarFilmes()

if (respostaAPI.status == 200) {
    def filmes = respostaAPI.filmes
    println("Total de filmes encontrados: ${filmes.size()}\n")
    
    filmes.each { filme ->
        println("Título: ${filme.titulo}")
        println("Ano de lançamento: ${filme.ano}")
        println("Gênero: ${filme.genero}")
        
        def elenco = filme.elenco
        if (elenco.size() > 0) {
            println("Elenco:")
            elenco.each { ator ->
                println("- ${ator}")
            }
        } else {
            println("Elenco não disponível.")
        }
        
        println("\n----\n")
    }
} else {
    println("Erro ao buscar filmes. Código de status: ${respostaAPI.status}")
}

def buscarFilmes() {
    // Simulação de chamada a uma API para buscar filmes
    def url = "https://api.streaming.com/filmes"
    def respostaAPI = new URL(url).getText()
    new JsonSlurper().parseText(respostaAPI)
}
```

Este código simula uma chamada a uma API de uma plataforma de streaming para obter informações sobre filmes. 

A função `buscarFilmes()` simula a chamada à API e retorna a resposta em formato JSON. 

Em seguida, verificamos se a resposta da API teve um código de status válido (200) e, caso positivo, exibimos as informações de cada filme. Para cada filme, imprimimos o título, o ano de lançamento e o gênero. Também verificamos se o elenco está disponível e, em caso afirmativo, imprimimos os nomes dos atores. 

O código é projetado para lidar com diferentes casos de dados retornados pela API, como elenco não disponível.

Espero que esse código complexo atenda às suas expectativas!