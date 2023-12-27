Claro! Aqui está um código complexo em R, que realiza uma análise de dados de vendas de uma empresa fictícia, utilizando gráficos e estatísticas descritivas:

```R
# Importando a biblioteca necessária
library(ggplot2)

# Lendo o arquivo de vendas
dados <- read.csv("caminho/do/arquivo.csv")

# Visualizando as primeiras linhas dos dados
head(dados)

# Calculando o total de vendas por mês
dados$Mes <- format(as.Date(dados$Data), "%Y-%m")
vendas_por_mes <- aggregate(dados$Valor, by = list(Mes = dados$Mes), FUN = sum)

# Plotando o gráfico de vendas por mês
ggplot(data = vendas_por_mes, aes(x = Mes, y = x)) +
  geom_line() +
  labs(x = "Mês", y = "Valor das Vendas", title = "Vendas Mensais") +
  theme_minimal()

# Calculando estatísticas descritivas das vendas
estatisticas <- summary(dados$Valor)
media <- mean(dados$Valor)
mediana <- median(dados$Valor)
desvio_padrao <- sd(dados$Valor)

# Exibindo as estatísticas descritivas
cat("Estatísticas Descritivas das Vendas:\n")
cat("Mínimo:", estatisticas[1], "\n")
cat("1º Quartil:", estatisticas[2], "\n")
cat("Mediana:", mediana, "\n")
cat("Média:", media, "\n")
cat("3º Quartil:", estatisticas[5], "\n")
cat("Máximo:", estatisticas[6], "\n")
cat("Desvio Padrão:", desvio_padrao, "\n")
```

Neste código, a biblioteca `ggplot2` é importada para a criação de gráficos. Em seguida, o arquivo de vendas é lido utilizando a função `read.csv()`. Os dados são exibidos utilizando a função `head()`.

Em seguida, é realizada uma manipulação dos dados para obter o total de vendas por mês. A variável `Mes` é criada para armazenar o mês e ano da venda, utilizando a função `format()` para formatar a data. A função `aggregate()` é utilizada para calcular a soma das vendas por mês.

Depois disso, um gráfico de linha é plotado utilizando o `ggplot2`, mostrando a evolução das vendas ao longo do tempo.

Em seguida, são calculadas algumas estatísticas descritivas das vendas, como mínimo, primeiro quartil, mediana, média, terceiro quartil, máximo e desvio padrão. A função `summary()` retorna um resumo estatístico dos dados, e as funções `mean()`, `median()` e `sd()` são utilizadas para calcular a média, mediana e desvio padrão, respectivamente.

Por fim, as estatísticas descritivas são exibidas utilizando a função `cat()`.