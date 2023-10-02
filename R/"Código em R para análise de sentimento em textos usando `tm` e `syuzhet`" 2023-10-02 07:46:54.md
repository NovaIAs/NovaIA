Claro! Aqui está um código complexo em R que realiza uma análise de sentimento em um conjunto de textos utilizando a biblioteca `tm`:

```R
# Instalando e carregando as bibliotecas necessárias
install.packages("tm")
install.packages("tidytext")
install.packages("syuzhet")
library(tm)
library(tidytext)
library(syuzhet)

# Criando um vetor de textos de exemplo
textos <- c("Eu amo esse filme!",
            "Que dia chato no trabalho.",
            "Estou muito feliz hoje.",
            "Esse livro é incrível!",
            "O serviço ao cliente foi horrível.")

# Criando um corpus e pré-processando os textos
corpus <- Corpus(VectorSource(textos))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
corpus <- tm_map(corpus, stripWhitespace)

# Criando uma matriz de termo-documento
tdm <- TermDocumentMatrix(corpus)

# Convertendo a matriz em um dataframe
df <- as.data.frame(as.matrix(tdm))

# Realizando a análise de sentimento com a biblioteca syuzhet
sentimentos <- get_sentiment(df$texts)

# Adicionando os valores de sentimento ao dataframe original
df$sentimento <- sentimentos

# Exibindo o resultado
print(df)
```

Este código utiliza a biblioteca `tm` para criar um corpus e pré-processar os textos, removendo pontuação, números e palavras irrelevantes. Em seguida, é criada uma matriz de termo-documento para representar os textos. O código também utiliza a biblioteca `syuzhet` para realizar uma análise de sentimento nos textos, atribuindo valores numéricos aos mesmos.

O resultado final é um dataframe que contém os textos originais e seus respectivos valores de sentimento. Este código pode ser adaptado para trabalhar com conjuntos de textos maiores e realizar análises mais avançadas de sentimentos.