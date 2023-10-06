Claro! Aqui está um código complexo em R que utiliza algumas funções avançadas para processar dados em um conjunto de amostras. Este código irá ler um arquivo CSV contendo informações de vendas e calcular a média, mediana, máximo e mínimo de cada produto vendido em um determinado período de tempo. Vamos lá:

```R
# Importar o arquivo CSV
dados <- read.csv("caminho/do/arquivo.csv")

# Filtrar as amostras por período de tempo
dados_filtrados <- subset(dados, data >= "2021-01-01" & data <= "2021-12-31")

# Criar uma lista vazia para armazenar os resultados
resultados <- list()

# Loop pelos produtos únicos
for (produto in unique(dados_filtrados$produto)) {
  
  # Filtrar as amostras apenas para o produto atual
  dados_produto <- subset(dados_filtrados, produto == produto)
  
  # Calcular as métricas: média, mediana, máximo e mínimo
  media <- mean(dados_produto$vendas)
  mediana <- median(dados_produto$vendas)
  maximo <- max(dados_produto$vendas)
  minimo <- min(dados_produto$vendas)
  
  # Armazenar as métricas em um data frame
  resultados[[produto]] <- data.frame(produto = produto, media = media, mediana = mediana, maximo = maximo, minimo = minimo)
}

# Unir todos os resultados em um único data frame
resultado_final <- do.call(rbind, resultados)

# Imprimir o resultado final
print(resultado_final)
```

Neste código, começamos importando o arquivo CSV contendo os dados de vendas usando a função `read.csv()`. Em seguida, filtramos as amostras com base em um período de tempo específico usando a função `subset()`.

Depois disso, criamos uma lista vazia chamada `resultados` para armazenar os resultados de cada produto. Em seguida, usamos um loop `for` para iterar sobre os produtos únicos presentes nos dados filtrados.

Dentro do loop, filtramos as amostras apenas para o produto atual e, em seguida, calculamos as métricas de interesse, como média, mediana, máximo e mínimo, usando as funções `mean()`, `median()`, `max()` e `min()`, respectivamente.

Em seguida, armazenamos as métricas em um data frame e adicionamos esse data frame à lista de resultados usando a sintaxe `resultados[[produto]] <- data.frame(...)`. Isso nos permite armazenar os resultados para cada produto separadamente.

Após o loop, usamos a função `do.call(rbind, resultados)` para unir todos os data frames de resultados em um único data frame chamado `resultado_final`.

Por fim, imprimimos o `resultado_final` com a função `print()` para exibir os resultados na tela.

Espero que este código complexo possa atender às suas necessidades!