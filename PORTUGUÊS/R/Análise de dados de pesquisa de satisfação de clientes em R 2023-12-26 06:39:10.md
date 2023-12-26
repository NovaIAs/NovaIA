Com certeza! Aqui está um código complexo em R que envolve a análise de dados de uma pesquisa de satisfação de clientes:

```R
# Importar os dados da pesquisa
dados <- read.csv("dados_pesquisa.csv", header = TRUE, sep = ";")

# Verificar a estrutura dos dados
str(dados)

# Calcular a média de satisfação geral dos clientes
media_satisfacao <- mean(dados$satisfacao_geral)

# Calcular a quantidade de clientes satisfeitos
clientes_satisfeitos <- subset(dados, satisfacao_geral >= 4)
qtde_clientes_satisfeitos <- nrow(clientes_satisfeitos)

# Calcular a quantidade de clientes insatisfeitos
clientes_insatisfeitos <- subset(dados, satisfacao_geral <= 2)
qtde_clientes_insatisfeitos <- nrow(clientes_insatisfeitos)

# Gerar um gráfico de barras para visualizar a distribuição da satisfação
library(ggplot2)
grafico_satisfacao <- ggplot(dados, aes(x = satisfacao_geral)) +
  geom_bar(fill = "blue") +
  labs(x = "Satisfação Geral", y = "Quantidade de Clientes", title = "Distribuição da Satisfação dos Clientes")

# Salvar o gráfico em um arquivo
ggsave("grafico_satisfacao.png", plot = grafico_satisfacao)

# Gerar um relatório com os resultados da pesquisa
relatorio <- data.frame(
  "Média de Satisfação" = media_satisfacao,
  "Clientes Satisfeitos" = qtde_clientes_satisfeitos,
  "Clientes Insatisfeitos" = qtde_clientes_insatisfeitos
)

# Exportar o relatório para um arquivo CSV
write.csv(relatorio, file = "relatorio_pesquisa.csv", row.names = FALSE)
```

Neste código, primeiro importamos os dados da pesquisa de satisfação dos clientes a partir de um arquivo CSV. Em seguida, verificamos a estrutura dos dados usando a função `str()`. 

Depois, calculamos a média de satisfação geral dos clientes e determinamos a quantidade de clientes satisfeitos e insatisfeitos. Utilizamos a função `subset()` para filtrar os clientes com satisfação maior ou igual a 4 (satisfeitos) e menor ou igual a 2 (insatisfeitos), e em seguida contamos o número de linhas com `nrow()`.

Para visualizar a distribuição da satisfação, utilizamos a biblioteca `ggplot2` para gerar um gráfico de barras. O gráfico é salvo em um arquivo chamado "grafico_satisfacao.png" usando a função `ggsave()`.

Por fim, geramos um relatório com os resultados da pesquisa, incluindo a média de satisfação, a quantidade de clientes satisfeitos e a quantidade de clientes insatisfeitos. O relatório é exportado para um arquivo CSV chamado "relatorio_pesquisa.csv" usando a função `write.csv()`.