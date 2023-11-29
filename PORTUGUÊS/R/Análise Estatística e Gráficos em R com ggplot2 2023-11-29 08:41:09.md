Claro! Aqui está um código complexo em linguagem R, que realiza uma análise estatística de dados e gera gráficos explicativos. 

```R
# Importação dos pacotes necessários
library(ggplot2)
library(dplyr)

# Leitura dos dados
dados <- read.csv("caminho/do/arquivo.csv")

# Limpeza dos dados
dados <- dados %>%
  filter(!is.na(coluna1) & !is.na(coluna2))

# Análise estatística
estatisticas <- dados %>%
  group_by(coluna1) %>%
  summarise(media_coluna2 = mean(coluna2),
            desvio_padrao_coluna2 = sd(coluna2),
            total_registros = n())

# Gráfico de dispersão
ggplot(data = dados, aes(x = coluna1, y = coluna2)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão",
       x = "Coluna 1",
       y = "Coluna 2") +
  theme_minimal()

# Gráfico de barras
ggplot(data = estatisticas, aes(x = coluna1, y = total_registros)) +
  geom_bar(stat = "identity", fill = "blue") +
  labs(title = "Gráfico de Barras",
       x = "Coluna 1",
       y = "Total de Registros") +
  theme_minimal()

# Gráfico de linha
ggplot(data = estatisticas, aes(x = coluna1, y = media_coluna2)) +
  geom_line(color = "red") +
  geom_point(color = "red") +
  labs(title = "Gráfico de Linha",
       x = "Coluna 1",
       y = "Média da Coluna 2") +
  theme_minimal()
```

Neste código, é necessário substituir "caminho/do/arquivo.csv" pelo caminho do arquivo CSV que contém os dados a serem analisados. Esse código realiza a leitura dos dados, limpa os registros com valores nulos, realiza uma análise estatística agrupando por uma coluna específica e, em seguida, gera três tipos diferentes de gráficos: um gráfico de dispersão, um gráfico de barras e um gráfico de linha.

Os gráficos são gerados utilizando a biblioteca ggplot2, que é amplamente utilizada para visualização de dados em R. Cada gráfico é personalizado com títulos, rótulos dos eixos e estilos minimalistas.