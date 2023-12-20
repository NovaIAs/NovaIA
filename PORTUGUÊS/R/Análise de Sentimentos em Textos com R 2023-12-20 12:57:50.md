Claro! Aqui está um código complexo em R que realiza uma análise de sentimentos em um conjunto de textos. Ele utiliza o pacote "tm" para processar os textos, o pacote "syuzhet" para extrair os sentimentos e o pacote "ggplot2" para visualização dos resultados.

```R
# Instalando e carregando os pacotes necessários
install.packages("tm")
install.packages("syuzhet")
install.packages("ggplot2")
library(tm)
library(syuzhet)
library(ggplot2)

# Definindo uma função para análise de sentimentos
analise_sentimentos <- function(textos) {
  # Pré-processamento dos textos
  corpus <- Corpus(VectorSource(textos))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Criando uma matriz de termos
  tdm <- TermDocumentMatrix(corpus)
  matriz <- as.matrix(tdm)
  
  # Extraindo os sentimentos dos textos
  sentimentos <- get_nrc_sentiment(textos)
  
  # Criando um data frame com os resultados
  resultados <- data.frame(texto = textos, 
                           alegria = sentimentos$joy,
                           medo = sentimentos$fear,
                           tristeza = sentimentos$sadness,
                           raiva = sentimentos$anger,
                           surpresa = sentimentos$surprise,
                           desgosto = sentimentos$disgust)
  
  # Plotando os resultados
  ggplot(resultados, aes(x = texto, y = alegria)) +
    geom_bar(stat = "identity", fill = "blue", width = 0.5) +
    labs(title = "Análise de Sentimentos",
         x = "Textos", y = "Alegria") +
    theme_minimal()
}

# Exemplo de uso da função
textos <- c("Eu amo esse filme!", 
            "Estou com medo de ir nessa montanha-russa.",
            "Fiquei triste com essa notícia.",
            "Estou com muita raiva desse político.",
            "Que surpresa agradável!",
            "Esse cheiro me dá nojo.")
analise_sentimentos(textos)
```

Este código demonstra como realizar uma análise de sentimentos em textos em português. Ele utiliza o pacote "tm" para pré-processamento dos textos, removendo pontuação, números e palavras irrelevantes. Em seguida, utiliza o pacote "syuzhet" para extrair os sentimentos dos textos, como alegria, medo, tristeza, raiva, surpresa e desgosto. Por fim, o pacote "ggplot2" é utilizado para plotar os resultados em um gráfico de barras, mostrando o sentimento de alegria em cada texto.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, por favor, me avise.