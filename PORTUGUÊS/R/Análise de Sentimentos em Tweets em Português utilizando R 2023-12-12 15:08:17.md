Claro! Aqui está um código complexo em R que realiza uma análise de sentimentos em um conjunto de tweets em português utilizando o pacote `text2vec` para pré-processamento e o modelo de análise de sentimentos `SentimentAnalysis`:

```R
# Instalando e carregando os pacotes necessários
install.packages("text2vec")
install.packages("SentimentAnalysis")
library(text2vec)
library(SentimentAnalysis)

# Carregando o conjunto de dados de tweets
tweets <- read.csv("tweets.csv", stringsAsFactors = FALSE)

# Pré-processamento dos tweets
corpus <- create_corpus(
  data_frame(tweets = tweets$text),
  text_field = "tweets",
  stopwords = stopwords_pt,
  token = ngram_tokenizer(min_n = 1, max_n = 2),
  preprocess = function(x) {
    x <- tolower(x)
    x <- removePunctuation(x)
    x <- removeNumbers(x)
    x <- removeWords(x, stopwords_pt)
    x <- stripWhitespace(x)
    return(x)
  }
)

# Criação da matriz de termos
it <- itoken(corpus$docs$text, progressbar = FALSE)
vocab <- create_vocabulary(it)
vectorizer <- vocab_vectorizer(vocab)
dtm <- create_dtm(it, vectorizer)

# Análise de sentimentos
sentiment <- SentimentAnalysis::analyzeSentiment(corpus$docs$text, language = "pt")
sentiment$score <- ifelse(sentiment$score < 0, "Negativo",
                          ifelse(sentiment$score == 0, "Neutro", "Positivo"))

# Exibindo os resultados
results <- data.frame(tweets = tweets$text, sentiment = sentiment$score)
print(results)
```

Nesse código, primeiro instalamos e carregamos os pacotes `text2vec` e `SentimentAnalysis`. Em seguida, lemos um conjunto de dados de tweets a partir de um arquivo CSV. 

Realizamos o pré-processamento dos tweets, convertendo-os para letras minúsculas, removendo pontuações, números e stopwords em português. 

Em seguida, criamos uma matriz de termos para representar os tweets e utilizamos o modelo `analyzeSentiment` do pacote `SentimentAnalysis` para realizar a análise de sentimentos. 

Finalmente, exibimos os resultados, que consistem nos tweets originais e sua classificação como "Negativo", "Neutro" ou "Positivo" com base no modelo de análise de sentimentos.