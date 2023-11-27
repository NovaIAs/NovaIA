Certamente! Aqui está um exemplo de código em R que realiza a análise de sentimentos em um conjunto de textos. Ele utiliza a biblioteca `tm` para pré-processamento textual, a biblioteca `tidytext` para a análise de sentimentos e a biblioteca `ggplot2` para visualização dos resultados. 

Primeiramente, é necessário instalar as bibliotecas necessárias. Caso não estejam instaladas, você pode utilizar o comando `install.packages` para instalá-las:

```R
install.packages("tm")
install.packages("tidytext")
install.packages("ggplot2")
```

Após a instalação, vamos importar as bibliotecas e carregar nossos dados de exemplo:

```R
library(tm)
library(tidytext)
library(ggplot2)

# Dados de exemplo
textos <- c("Eu adorei o novo filme!",
            "O serviço ao cliente foi péssimo.",
            "A comida estava deliciosa.",
            "O tempo estava horrível hoje.",
            "O livro me deixou intrigado.",
            "Eu não gostei do atendimento.")
```

Em seguida, realizamos o pré-processamento dos textos, removendo caracteres especiais, números e transformando todas as palavras em minúsculas:

```R
# Pré-processamento dos textos
corpus <- Corpus(VectorSource(textos))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removeWords, stopwords("portuguese"))
corpus <- tm_map(corpus, stripWhitespace)
```

Agora, vamos realizar a análise de sentimentos dos textos utilizando o dicionário de palavras do `tidytext`:

```R
# Análise de sentimentos
sentimentos <- get_sentiments("brazilian.port") # Dicionário de sentimentos em português
dados_sentimentos <- tidy(corpus) %>%
  inner_join(sentimentos)

# Visualização dos resultados
ggplot(dados_sentimentos, aes(x = sentiment, fill = sentiment)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Sentimento", y = "Frequência", title = "Análise de Sentimentos")
```

Este código realiza a análise de sentimentos nos textos de exemplo e gera um gráfico de barras mostrando a frequência dos diferentes sentimentos encontrados. É importante destacar que é possível utilizar outros dicionários de palavras ou criar um personalizado para melhor adequar à sua aplicação.

Espero que esse código seja útil para você!