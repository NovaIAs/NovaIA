Claro! Aqui está um exemplo de um código complexo em R:

```R
# Código para análise de dados de vendas

# Carregar as bibliotecas necessárias
library(dplyr)
library(ggplot2)

# Importar os dados de vendas
dados_vendas <- read.csv("dados_vendas.csv")

# Limpar os dados
dados_vendas <- dados_vendas %>%
  filter(!is.na(valor_venda))

# Criar uma nova coluna de data
dados_vendas$data <- as.Date(dados_vendas$data, format = "%d/%m/%Y")

# Calcular a receita total
receita_total <- sum(dados_vendas$valor_venda)

# Calcular a média de vendas diárias
media_vendas_diarias <- nrow(dados_vendas) / length(unique(dados_vendas$data))

# Calcular a receita média por produto
receita_media_produto <- dados_vendas %>%
  group_by(produto) %>%
  summarise(receita_media = mean(valor_venda))

# Criar um gráfico de barras da receita média por produto
ggplot(receita_media_produto, aes(x = produto, y = receita_media)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Receita Média por Produto",
       x = "Produto",
       y = "Receita Média")

# Salvar o gráfico em um arquivo
ggsave("grafico_receita_media.png", width = 10, height = 8, dpi = 300)

# Gerar um relatório com os resultados
relatorio <- data.frame(Receita_Total = receita_total,
                        Media_Vendas_Diarias = media_vendas_diarias)

write.csv(relatorio, "relatorio_resultados.csv", row.names = FALSE)
```

Neste código, estamos realizando uma análise de dados de vendas. Primeiro, carregamos as bibliotecas `dplyr` e `ggplot2`, que nos ajudarão na manipulação dos dados e na criação do gráfico.

Em seguida, importamos os dados de vendas a partir de um arquivo CSV chamado "dados_vendas.csv".

Depois, limpamos os dados, removendo linhas que contenham valores nulos na coluna "valor_venda".

Criamos uma nova coluna "data", convertendo a coluna existente em formato de data.

Calculamos a receita total, somando os valores de venda.

Calculamos a média de vendas diárias, dividindo o número total de vendas pela quantidade de dias distintos.

Calculamos a receita média por produto, agrupando os dados pela coluna "produto" e calculando a média dos valores de venda para cada grupo.

Criamos um gráfico de barras utilizando a função `ggplot`, representando a receita média por produto.

Salvamos esse gráfico em um arquivo PNG chamado "grafico_receita_media.png".

Por fim, geramos um relatório com os resultados da análise, criando um data frame com as informações de receita total e média de vendas diárias, e salvando-o em um arquivo CSV chamado "relatorio_resultados.csv".