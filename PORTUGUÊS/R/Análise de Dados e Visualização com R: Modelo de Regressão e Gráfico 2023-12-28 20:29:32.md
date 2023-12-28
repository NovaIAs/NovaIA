Claro! Aqui está um código complexo em R que envolve análise de dados e visualização. Ele cria um modelo de regressão e plota um gráfico com os resultados:

```R
# Importar pacotes necessários
library(ggplot2)
library(dplyr)

# Importar conjunto de dados
dados <- read.csv("caminho/para/o/arquivo.csv")

# Limpar os dados
dados <- dados %>%
  filter(!is.na(variavel1), !is.na(variavel2)) %>%
  select(variavel1, variavel2)

# Dividir os dados em conjunto de treinamento e teste
set.seed(123)
indices_treinamento <- sample(1:nrow(dados), nrow(dados) * 0.8)
dados_treinamento <- dados[indices_treinamento, ]
dados_teste <- dados[-indices_treinamento, ]

# Criar modelo de regressão linear
modelo <- lm(variavel1 ~ variavel2, data = dados_treinamento)

# Fazer previsões com o modelo
previsoes <- predict(modelo, newdata = dados_teste)

# Calcular a raiz do erro quadrático médio
rmse <- sqrt(mean((previsoes - dados_teste$variavel1)^2))

# Plotar gráfico com os dados e a linha de regressão
ggplot(dados, aes(x = variavel2, y = variavel1)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Variável 2", y = "Variável 1") +
  theme_bw()

# Imprimir o valor do RMSE
cat("RMSE:", rmse)
```

Este código realiza as seguintes etapas:

1. Importa os pacotes necessários (ggplot2 e dplyr);
2. Carrega o conjunto de dados a partir de um arquivo CSV;
3. Realiza a limpeza dos dados, removendo linhas com valores ausentes e selecionando apenas as variáveis relevantes;
4. Divide os dados em um conjunto de treinamento (80%) e um conjunto de teste (20%);
5. Cria um modelo de regressão linear utilizando a variável1 como variável dependente e a variável2 como variável independente;
6. Faz previsões utilizando o modelo criado com base nos dados de teste;
7. Calcula a raiz do erro quadrático médio (RMSE) entre as previsões e os valores reais;
8. Plota um gráfico com os dados e a linha de regressão;
9. Imprime o valor do RMSE.

Espero que este código atenda às suas necessidades!