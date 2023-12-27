Claro! Aqui está um exemplo de um código complexo em R que envolve manipulação de dados, visualização e modelagem estatística. Vamos criar um programa que analisa dados de vendas de uma empresa e gera previsões de vendas futuras.

```R
# Importar bibliotecas necessárias
library(tidyverse)
library(lubridate)
library(forecast)

# Importar e preparar os dados
dados <- read_csv("caminho/do/arquivo.csv")
dados <- dados %>%
  mutate(data_venda = dmy(data_venda)) %>%
  arrange(data_venda)

# Análise exploratória dos dados
vendas_por_mes <- dados %>%
  group_by(mes = month(data_venda)) %>%
  summarise(total_vendas = sum(valor_venda))

# Visualização dos dados
ggplot(vendas_por_mes, aes(x = mes, y = total_vendas)) +
  geom_line() +
  labs(x = "Mês", y = "Total de Vendas", title = "Vendas por Mês")

# Modelagem estatística e previsão de vendas futuras
modelo <- forecast::auto.arima(vendas_por_mes$total_vendas)
previsao <- forecast::forecast(modelo, h = 12)

# Visualização da previsão de vendas
plot(previsao, main = "Previsão de Vendas Futuras")

# Salvar a previsão em um arquivo CSV
write.csv(previsao$mean, file = "previsao_vendas.csv", row.names = FALSE)
```

Neste código, começamos importando as bibliotecas necessárias: tidyverse para manipulação de dados, lubridate para lidar com datas e forecast para modelagem estatística e previsão. Em seguida, importamos os dados de vendas de um arquivo CSV e os preparamos para análise, convertendo a coluna de data para o formato correto e ordenando os dados por data.

Em seguida, realizamos uma análise exploratória dos dados, agrupando as vendas por mês e calculando o total de vendas em cada mês. Depois, usamos a biblioteca ggplot para visualizar os dados em um gráfico de linhas.

Na etapa de modelagem estatística, utilizamos a função `auto.arima` para encontrar automaticamente o modelo ARIMA mais adequado para os dados de vendas por mês. Em seguida, usamos a função `forecast` para gerar previsões de vendas futuras para os próximos 12 meses.

Por fim, plotamos a previsão de vendas em um gráfico e salvamos os valores previstos em um arquivo CSV.

Lembre-se de substituir "caminho/do/arquivo.csv" pelo caminho do seu arquivo de dados e ajustar os nomes das colunas de acordo com a estrutura do seu conjunto de dados.