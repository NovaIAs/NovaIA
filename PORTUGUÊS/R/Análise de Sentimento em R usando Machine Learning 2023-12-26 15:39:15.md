Claro! Aqui está um exemplo de um código complexo em R que faz a análise de sentimento de um conjunto de textos usando um modelo de Machine Learning. O código utiliza bibliotecas como `tm`, `RWeka` e `e1071` para processar os textos, criar o modelo e realizar a classificação de sentimentos.

```R
# Instalar e carregar as bibliotecas necessárias
install.packages("tm")
install.packages("RWeka")
install.packages("e1071")
library(tm)
library(RWeka)
library(e1071)

# Função para pré-processar os textos
preprocessText <- function(text) {
  # Transformar o texto em minúsculas
  text <- tolower(text)
  
  # Remover pontuação
  text <- removePunctuation(text)
  
  # Remover números
  text <- removeNumbers(text)
  
  # Remover stopwords
  text <- removeWords(text, stopwords("portuguese"))
  
  # Remover espaços em branco extras
  text <- stripWhitespace(text)
  
  return(text)
}

# Carregar o conjunto de textos
textos <- c("Eu amo essa música!", "Que filme chato...", "O jantar estava delicioso.",
            "Não gostei do atendimento.", "O livro é incrível!", "A festa foi um desastre.")

# Pré-processar os textos
textosPreprocessados <- lapply(textos, preprocessText)

# Criar uma matriz de termos
corpus <- Corpus(VectorSource(textosPreprocessados))
termMatrix <- DocumentTermMatrix(corpus)

# Criar a matriz de classificação de sentimentos
sentimentos <- c("positivo", "negativo", "positivo", "negativo", "positivo", "negativo")
classMatrix <- as.matrix(factor(sentimentos, levels = c("positivo", "negativo")))

# Criar o modelo de classificação
modelo <- naiveBayes(termMatrix, classMatrix)

# Classificar novos textos
novosTextos <- c("Adorei o novo restaurante!", "Que dia horrível...")
novosTextosPreprocessados <- lapply(novosTextos, preprocessText)
novosTermMatrix <- DocumentTermMatrix(Corpus(VectorSource(novosTextosPreprocessados)))
classificacao <- predict(modelo, novosTermMatrix)

# Imprimir os resultados
for (i in 1:length(novosTextos)) {
  print(paste("Texto:", novosTextos[i]))
  print(paste("Sentimento:", ifelse(classificacao[i] == "positivo", "Positivo", "Negativo")))
  print("----------------------")
}
```

Neste código, primeiro instalamos e carregamos as bibliotecas necessárias. Em seguida, definimos uma função `preprocessText()` que realiza o pré-processamento dos textos, convertendo-os para minúsculas, removendo pontuações, números, stopwords e espaços em branco extras.

Depois, carregamos o conjunto de textos e aplicamos a função `preprocessText()` a cada um deles. Em seguida, criamos uma matriz de termos usando a função `DocumentTermMatrix()`, que representa a frequência de cada termo nos textos.

Em seguida, definimos uma matriz de classificação de sentimentos, onde cada texto é classificado como positivo ou negativo. Criamos o modelo de classificação usando o algoritmo Naive Bayes com a função `naiveBayes()`.

Por fim, classificamos novos textos usando o modelo criado e imprimimos os resultados. Os novos textos são pré-processados, transformados em matriz de termos e classificados usando a função `predict()`. Em seguida, os resultados são impressos na tela.

Esse é um exemplo de código complexo em R que realiza a análise de sentimento de textos. É um código grande e diferenciado, que pode ser adaptado para diferentes conjuntos de dados e aplicações.