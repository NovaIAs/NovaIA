Claro! Aqui está um exemplo de um código complexo em Groovy que realiza a análise de sentimentos de um texto usando uma API de processamento de linguagem natural:

```groovy
@Grab(group='com.mashape.unirest', module='unirest-java', version='1.4.9')
import com.mashape.unirest.http.Unirest
import groovy.json.JsonSlurper

def analisarSentimento(texto) {
    def apiKey = "SUA_API_KEY_AQUI"
    def url = "https://api.sentimentanalysis.io/v1/analyze"
    
    def resposta = Unirest.post(url)
                          .header("Content-Type", "application/json")
                          .header("Authorization", "Bearer " + apiKey)
                          .body([text: texto])
                          .asString()
                          
    def json = new JsonSlurper().parseText(resposta.body)
    
    def sentimento = json.sentiment
    def pontuacao = json.sentiment_score
    
    println "Texto: $texto"
    println "Sentimento: $sentimento"
    println "Pontuação: $pontuacao"
}

def main() {
    def texto1 = "Eu amo programar em Groovy!"
    def texto2 = "Estou frustrado com meu código."
    def texto3 = "Hoje está um dia lindo!"
    
    analisarSentimento(texto1)
    analisarSentimento(texto2)
    analisarSentimento(texto3)
}

main()
```

Neste código, utilizamos a biblioteca `unirest-java` para realizar uma requisição HTTP POST para a API de análise de sentimentos `sentimentanalysis.io`. É necessário substituir `SUA_API_KEY_AQUI` pela sua chave de API válida.

A função `analisarSentimento` recebe um texto como parâmetro e envia uma requisição para a API, fornecendo o texto a ser analisado. Em seguida, o JSON de resposta é convertido em objeto usando o `JsonSlurper`.

O código principal `main` chama a função `analisarSentimento` com três textos diferentes como exemplo. O resultado é impresso no console, exibindo o texto, o sentimento detectado (positivo, negativo ou neutro) e a pontuação associada a esse sentimento.

Esse código mostra um exemplo de como utilizar uma API externa para realizar análise de sentimentos em textos usando Groovy. Ele é apenas uma demonstração e pode ser personalizado e adaptado de acordo com suas necessidades específicas.