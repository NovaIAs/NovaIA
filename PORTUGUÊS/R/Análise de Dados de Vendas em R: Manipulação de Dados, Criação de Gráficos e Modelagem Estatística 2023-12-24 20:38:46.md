Claro! Aqui está um código complexo em R, que envolve manipulação de dados, criação de gráficos e modelagem estatística. Ele é um exemplo fictício de análise de dados de vendas de uma empresa:

```R
# Carregando bibliotecas necessárias
library(ggplot2)
library(dplyr)
library(lmtest)

# Importando dados de vendas
dados <- read.csv("caminho/para/o/arquivo.csv")

# Removendo valores faltantes
dados <- na.omit(dados)

# Transformando a variável data em formato de data
dados$data <- as.Date(dados$data, format = "%d/%m/%Y")

# Criando uma nova variável para o ano
dados$ano <- format(dados$data, format = "%Y")

# Criando uma nova variável para o mês
dados$mes <- format(dados$data, format = "%m")

# Agregando as vendas mensais
vendas_mensais <- dados %>%
  group_by(ano, mes) %>%
  summarise(total_vendas = sum(valor_venda))

# Criando um gráfico de barras das vendas mensais
ggplot(vendas_mensais, aes(x = mes, y = total_vendas, fill = ano)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Vendas Mensais", x = "Mês", y = "Total de Vendas") +
  theme_minimal()

# Ajustando um modelo de regressão linear
modelo <- lm(total_vendas ~ mes, data = vendas_mensais)

# Realizando o teste de significância dos coeficientes
coeftest(modelo)

# Previsão de vendas para o próximo mês
prox_mes <- max(vendas_mensais$mes) + 1
prox_ano <- max(vendas_mensais$ano)

previsao <- data.frame(mes = prox_mes, ano = prox_ano)

vendas_previstas <- predict(modelo, newdata = previsao)

print(paste("A previsão de vendas para o próximo mês é:", vendas_previstas))
```

Neste código, começamos carregando as bibliotecas necessárias, como ggplot2, dplyr e lmtest. Em seguida, importamos os dados de vendas de um arquivo CSV e removemos quaisquer valores faltantes.

Depois disso, transformamos a variável data em formato de data e criamos duas novas variáveis: uma para o ano e outra para o mês. Usando a função `group_by` e `summarise` do pacote dplyr, agregamos as vendas mensais.

Em seguida, criamos um gráfico de barras utilizando o pacote ggplot2, onde as barras representam as vendas mensais e a cor indica o ano.

Em seguida, ajustamos um modelo de regressão linear usando a função `lm` e realizamos um teste de significância dos coeficientes usando a função `coeftest` do pacote lmtest.

Por fim, fazemos uma previsão das vendas para o próximo mês usando o modelo ajustado e imprimimos o resultado na tela.

Este código é apenas um exemplo de como você pode realizar uma análise de dados complexa em R. É importante ressaltar que o código pode ser adaptado de acordo com a estrutura dos seus dados e necessidades específicas.