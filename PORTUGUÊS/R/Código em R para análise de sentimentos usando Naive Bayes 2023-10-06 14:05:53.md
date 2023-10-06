Claro! Abaixo está um código complexo em R que realiza uma análise de sentimento em um conjunto de textos usando o algoritmo Naive Bayes.

```R
# Carregando as bibliotecas necessárias
library(tm)
library(e1071)

# Definindo uma função para pré-processar os textos
preprocess_text <- function(text) {
  # Converter para minúsculas
  text <- tolower(text)
  
  # Remover pontuações
  text <- gsub("[[:punct:]]", " ", text)
  
  # Remover números
  text <- gsub("[[:digit:]]", " ", text)
  
  # Remover espaços em branco adicionais
  text <- gsub("\\s+", " ", text)
  
  # Retornar o texto pré-processado
  return(text)
}

# Carregando os textos de treinamento e teste
train_texts <- c("Eu amo esse filme!", "Esse livro é incrível!", "Que música maravilhosa!")
train_labels <- c("positivo", "positivo", "positivo")
test_texts <- c("Que péssimo serviço!", "Não gostei desse produto.", "O show foi decepcionante.")
test_labels <- c("negativo", "negativo", "negativo")

# Pré-processando os textos de treinamento e teste
train_texts <- lapply(train_texts, preprocess_text)
test_texts <- lapply(test_texts, preprocess_text)

# Criando um corpus
corpus <- Corpus(VectorSource(train_texts))

# Pré-processando o corpus
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stripWhitespace)

# Criando uma matriz de termos-documentos
dtm <- DocumentTermMatrix(corpus)

# Convertendo a matriz em um data frame
train_data <- as.data.frame(as.matrix(dtm))
train_data$label <- train_labels

# Treinando o modelo Naive Bayes
model <- naiveBayes(label ~ ., data = train_data)

# Pré-processando os textos de teste
test_data <- lapply(test_texts, function(text) {
  text_vector <- DocumentTermMatrix(Corpus(VectorSource(text)), control = list(dictionary = Terms(dtm)))
  as.data.frame(as.matrix(text_vector))
})

# Realizando a classificação dos textos de teste
predicted_labels <- sapply(test_data, function(data) {
  predict(model, data)
})

# Exibindo os resultados
for (i in seq_along(predicted_labels)) {
  cat("Texto:", test_texts[[i]], "\n")
  cat("Sentimento esperado:", test_labels[i], "\n")
  cat("Sentimento previsto:", predicted_labels[i], "\n\n")
}
```

Neste código, utilizamos o pacote `tm` para realizar a pré-processamento dos textos, removendo pontuações, números, palavras irrelevantes e convertendo para minúsculas. Em seguida, criamos uma matriz de termos-documentos que representa os textos de treinamento. Com base nessa matriz, treinamos um modelo Naive Bayes usando o pacote `e1071`.

Na parte de teste, aplicamos o mesmo pré-processamento nos textos de teste e os convertemos em matrizes de termos-documentos usando o mesmo vocabulário utilizado nos textos de treinamento. Por fim, classificamos os textos de teste usando o modelo treinado e exibimos os resultados.

Esse código realiza uma tarefa de análise de sentimento, onde textos são classificados como positivos ou negativos. Ele pode ser útil em aplicações como análise de feedback de clientes ou monitoramento de redes sociais.