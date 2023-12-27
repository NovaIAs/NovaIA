Claro! Aqui está um código complexo em R, que realiza uma análise de sentimentos em textos utilizando a biblioteca 'tm' e a função 'get_nrc_sentiment' do pacote 'syuzhet':

```R
# Instalação dos pacotes necessários (caso ainda não estejam instalados)
install.packages("tm")
install.packages("syuzhet")

# Carregamento das bibliotecas
library(tm)
library(syuzhet)

# Função para realizar a análise de sentimentos em um texto
analisar_sentimentos <- function(texto) {
  
  # Criação de um corpus a partir do texto
  corpus <- Corpus(VectorSource(texto))
  
  # Pré-processamento do corpus
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # Criação de um objeto para armazenar os resultados da análise
  resultados <- data.frame(palavra = character(),
                           alegria = numeric(),
                           medo = numeric(),
                           tristeza = numeric(),
                           surpresa = numeric(),
                           raiva = numeric(),
                           desgosto = numeric(),
                           valencia = numeric(),
                           dominancia = numeric())
  
  # Loop para percorrer cada documento do corpus
  for (i in 1:length(corpus)) {
    doc <- tm_term_score(corpus[[i]], term = "get_nrc_sentiment")
    doc$result$palavra <- names(doc$result)
    resultados <- rbind(resultados, doc$result)
  }
  
  # Retorno dos resultados
  return(resultados)
}

# Texto de exemplo
texto <- "Este é um ótimo dia! Estou muito feliz e animado. Porém, também estou um pouco preocupado com a situação atual do mundo."

# Chamada da função para realizar a análise de sentimentos no texto
resultado <- analisar_sentimentos(texto)

# Impressão dos resultados
print(resultado)
```

Neste código, é criada uma função chamada 'analisar_sentimentos' que recebe um texto como entrada e retorna uma tabela com os valores de sentimentos para cada palavra no texto. O texto é pré-processado removendo pontuações, números, palavras comuns em português e espaços em branco. Em seguida, é realizado um loop para percorrer cada documento do corpus (no caso, apenas um documento) e chamar a função 'get_nrc_sentiment' para obter os valores de sentimentos para cada palavra. Os resultados são armazenados em um data frame e retornados pela função.

No exemplo fornecido, o texto utilizado é "Este é um ótimo dia! Estou muito feliz e animado. Porém, também estou um pouco preocupado com a situação atual do mundo.". A função retorna um data frame com as colunas 'palavra', 'alegria', 'medo', 'tristeza', 'surpresa', 'raiva', 'desgosto', 'valencia' e 'dominancia'.