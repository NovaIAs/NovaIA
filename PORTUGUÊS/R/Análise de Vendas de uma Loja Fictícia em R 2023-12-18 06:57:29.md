Claro! Aqui está um código complexo em R que realiza uma análise de dados de vendas de uma loja fictícia:

```R
# Carregar pacotes necessários
library(dplyr)
library(ggplot2)
library(lubridate)

# Importar dados de vendas
dados_vendas <- read.csv("dados_vendas.csv", stringsAsFactors = FALSE)

# Converter coluna de datas para o formato correto
dados_vendas$data <- as.Date(dados_vendas$data, format = "%d/%m/%Y")

# Filtrar dados para o último ano
dados_vendas <- dados_vendas %>%
  filter(data >= Sys.Date() - years(1))

# Calcular total de vendas por mês e por produto
vendas_por_mes_produto <- dados_vendas %>%
  group_by(mes = format(data, "%Y-%m"), produto) %>%
  summarise(total_vendas = sum(quantidade))

# Criar gráfico de barras para visualizar as vendas por mês e por produto
ggplot(vendas_por_mes_produto, aes(x = mes, y = total_vendas, fill = produto)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Mês", y = "Total de Vendas", title = "Vendas por Mês e Produto") +
  theme_minimal()

# Calcular média de vendas por dia da semana
vendas_por_dia_semana <- dados_vendas %>%
  mutate(dia_semana = weekdays(data)) %>%
  group_by(dia_semana) %>%
  summarise(media_vendas = mean(quantidade))

# Criar gráfico de linha para visualizar a média de vendas por dia da semana
ggplot(vendas_por_dia_semana, aes(x = dia_semana, y = media_vendas)) +
  geom_line() +
  labs(x = "Dia da Semana", y = "Média de Vendas", title = "Média de Vendas por Dia da Semana") +
  theme_minimal()

# Calcular total de vendas por vendedor
vendas_por_vendedor <- dados_vendas %>%
  group_by(vendedor) %>%
  summarise(total_vendas = sum(quantidade))

# Ordenar vendedores por total de vendas decrescente
vendas_por_vendedor <- vendas_por_vendedor %>%
  arrange(desc(total_vendas))

# Imprimir tabela com os vendedores e seus respectivos totais de vendas
print(vendas_por_vendedor)
```

Neste código, as etapas são as seguintes:

1. Carregar os pacotes necessários: dplyr, ggplot2 e lubridate.
2. Importar os dados de vendas de um arquivo CSV chamado "dados_vendas.csv".
3. Converter a coluna de datas para o formato correto.
4. Filtrar os dados para considerar apenas o último ano.
5. Calcular o total de vendas por mês e por produto.
6. Criar um gráfico de barras para visualizar as vendas por mês e por produto.
7. Calcular a média de vendas por dia da semana.
8. Criar um gráfico de linha para visualizar a média de vendas por dia da semana.
9. Calcular o total de vendas por vendedor.
10. Ordenar os vendedores pelo total de vendas decrescente.
11. Imprimir uma tabela com os vendedores e seus respectivos totais de vendas.

Esse código realiza uma análise de dados completa, desde a importação e manipulação dos dados até a visualização dos resultados em gráficos e tabelas.