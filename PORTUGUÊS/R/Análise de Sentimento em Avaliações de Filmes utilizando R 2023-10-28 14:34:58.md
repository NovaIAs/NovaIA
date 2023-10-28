Claro! Aqui está um código complexo em R que realiza uma análise de sentimento em um conjunto de dados de avaliações de filmes:

```R
# Instalação dos pacotes necessários
install.packages(c("tm", "stringr", "syuzhet", "dplyr"))

# Carregamento das bibliotecas
library(tm)
library(stringr)
library(syuzhet)
library(dplyr)

# Função para remover caracteres especiais e transformar texto em minúsculo
clean_text <- function(text) {
  text <- tolower(text)
  text <- str_replace_all(text, "[^[:alnum:][:space:]çãõáéíóúâêîôûàèìòùäëïöü]+", " ")
  text <- str_squish(text)
  return(text)
}

# Leitura do conjunto de dados
data <- read.csv("avaliacoes_filmes.csv", stringsAsFactors = FALSE)

# Pré-processamento dos dados
data$texto <- sapply(data$texto, clean_text)

# Criação de um corpus com os textos das avaliações
corpus <- Corpus(VectorSource(data$texto))

# Remoção de stopwords
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))

# Cálculo dos sentimentos das avaliações
sentiments <- get_sentiment(corpus, method = "syuzhet", plot = FALSE)

# Adição dos sentimentos ao conjunto de dados
data$sentimento <- sentiments

# Análise dos sentimentos
sentiment_summary <- data %>%
  group_by(filme) %>%
  summarize(media_sentimento = mean(sentimento),
            total_avaliacoes = n())

# Classificação dos filmes por sentimento médio
sentiment_summary <- sentiment_summary %>%
  mutate(classificacao = case_when(media_sentimento >= 0.5 ~ "Positivo",
                                   media_sentimento < 0.5 ~ "Negativo"))

# Ordenação dos filmes pela classificação
sentiment_summary <- sentiment_summary %>%
  arrange(desc(media_sentimento))

# Exibição dos resultados
print(sentiment_summary)
```

Neste código, primeiro são instalados e carregados os pacotes necessários para realizar a análise de sentimento. Em seguida, é definida uma função `clean_text` que remove caracteres especiais e transforma o texto em minúsculo.

O conjunto de dados de avaliações de filmes é lido a partir de um arquivo CSV e passa pelo pré-processamento, onde cada texto é limpo utilizando a função `clean_text` e as stopwords em português são removidas.

É criado um corpus com os textos das avaliações e, em seguida, é calculado o sentimento de cada avaliação utilizando o método "syuzhet", que está disponível no pacote `syuzhet`. O resultado é adicionado ao conjunto de dados original.

Após obter os sentimentos das avaliações, é feita uma análise dos sentimentos por filme, calculando a média do sentimento e o total de avaliações para cada filme.

Os filmes são então classificados como "Positivo" ou "Negativo" com base em um limite de sentimento médio de 0.5. Por fim, os resultados são ordenados pela classificação e exibidos.

Lembre-se de substituir o nome do arquivo CSV (avaliacoes_filmes.csv) pelo nome correto do seu arquivo de dados.