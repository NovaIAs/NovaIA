Claro! Aqui está um código complexo em R que envolve manipulação de dados, análise estatística e visualização:

```R
# Carregar as bibliotecas necessárias
library(dplyr)
library(ggplot2)
library(tidyr)

# Ler o conjunto de dados
dados <- read.csv("caminho_do_arquivo.csv")

# Pré-processamento dos dados
dados <- dados %>%
  filter(!is.na(variavel1) & !is.na(variavel2)) %>%
  mutate(variavel3 = variavel1 + variavel2)

# Análise estatística descritiva
sumario_estatistico <- dados %>%
  summarise(media_variavel1 = mean(variavel1),
            media_variavel2 = mean(variavel2),
            desvio_padrao_variavel1 = sd(variavel1),
            desvio_padrao_variavel2 = sd(variavel2))

# Visualização dos dados
grafico <- dados %>%
  gather(key = "variavel", value = "valor", variavel1, variavel2) %>%
  ggplot(aes(x = variavel, y = valor)) +
  geom_boxplot() +
  theme_bw()

# Salvar o sumário estatístico em um arquivo
write.csv(sumario_estatistico, "sumario_estatistico.csv", row.names = FALSE)

# Salvar o gráfico em um arquivo
ggsave("grafico.png", plot = grafico, dpi = 300, width = 8, height = 6)
```

Neste código, assumimos que você possui um arquivo CSV contendo dados com duas variáveis chamadas "variavel1" e "variavel2". Primeiramente, carregamos as bibliotecas necessárias para executar as operações. Em seguida, lemos o arquivo CSV e realizamos pré-processamento dos dados, filtrando linhas com valores ausentes e criando uma nova variável chamada "variavel3".

Em seguida, realizamos uma análise estatística descritiva das variáveis usando a função `summarise` do pacote dplyr. Calculamos a média e o desvio padrão de cada variável e salvamos o resultado em um arquivo CSV chamado "sumario_estatistico.csv".

Por fim, criamos um gráfico de caixa (boxplot) para visualizar a distribuição dos valores das variáveis usando a função `ggplot` do pacote ggplot2. O gráfico é salvo em um arquivo PNG chamado "grafico.png" com resolução de 300 DPI e dimensões de 8x6 polegadas.

Este código é apenas um exemplo de como você pode realizar análise e visualização de dados usando a linguagem R. É claro que ele pode ser adaptado para atender às suas necessidades específicas.