Claro! Aqui está um exemplo de código complexo em R:

```R
# Código complexo em R
# Este código realiza uma análise estatística de dados de vendas e gera visualizações gráficas

# Importar bibliotecas
library(ggplot2)
library(dplyr)

# Ler o arquivo de dados
dados <- read.csv("dados_vendas.csv", header = TRUE)

# Limpar dados faltantes
dados <- na.omit(dados)

# Calcular total de vendas por mês
vendas_mes <- dados %>%
  group_by(mes) %>%
  summarize(total_vendas = sum(valor_venda))

# Calcular média e desvio padrão das vendas por categoria
vendas_categoria <- dados %>%
  group_by(categoria) %>%
  summarize(media_vendas = mean(valor_venda),
            desvio_padrao = sd(valor_venda))

# Criar visualização gráfica da evolução das vendas mensais
grafico_mes <- ggplot(vendas_mes, aes(x = mes, y = total_vendas)) +
  geom_line() +
  geom_point() +
  labs(title = "Evolução das vendas mensais",
       x = "Mês",
       y = "Total de vendas")

# Criar visualização gráfica das vendas por categoria
grafico_categoria <- ggplot(vendas_categoria, aes(x = categoria, y = media_vendas)) +
  geom_bar(stat = "identity") +
  labs(title = "Vendas por categoria",
       x = "Categoria",
       y = "Média de vendas")

# Salvar gráficos em arquivos
ggsave("grafico_mes.png", plot = grafico_mes, width = 6, height = 4, dpi = 300)
ggsave("grafico_categoria.png", plot = grafico_categoria, width = 6, height = 4, dpi = 300)

# Imprimir resultados
print("Total de vendas por mês:")
print(vendas_mes)
print("Média e desvio padrão das vendas por categoria:")
print(vendas_categoria)
```

Neste código em R, estamos realizando uma análise estatística de dados de vendas. Primeiro, importamos as bibliotecas necessárias (ggplot2 e dplyr) e, em seguida, lemos um arquivo CSV contendo os dados de vendas.

Em seguida, o código limpa os dados faltantes usando a função `na.omit`. Depois, calculamos o total de vendas por mês usando a função `group_by` e `summarize` do pacote dplyr.

Também calculamos a média e o desvio padrão das vendas por categoria usando as mesmas funções do pacote dplyr.

Em seguida, criamos duas visualizações gráficas usando a biblioteca ggplot2. A primeira visualização mostra a evolução das vendas mensais em um gráfico de linha e pontos. A segunda visualização mostra as vendas médias por categoria em um gráfico de barras.

Por fim, salvamos os gráficos em arquivos PNG usando a função `ggsave` e imprimimos os resultados na tela usando a função `print`.

Espero que este código atenda às suas expectativas de complexidade!