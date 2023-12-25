Claro! Aqui está um código complexo em R que envolve a manipulação de dados e a criação de gráficos:

```R
# Carregando o pacote tidyverse
library(tidyverse)

# Lendo o arquivo de dados
dados <- read_csv("caminho_para_o_arquivo.csv")

# Limpeza e pré-processamento dos dados
dados_limpos <- dados %>%
  filter(!is.na(coluna_desejada)) %>%
  mutate(coluna_desejada = as.character(coluna_desejada)) %>%
  mutate(coluna_desejada = str_replace_all(coluna_desejada, "padrao_indesejado", "padrao_desejado")) %>%
  select(coluna_desejada, coluna_interesse)

# Análise exploratória dos dados
resumo_estatistico <- dados_limpos %>%
  group_by(coluna_desejada) %>%
  summarize(media = mean(coluna_interesse),
            desvio_padrao = sd(coluna_interesse),
            mediana = median(coluna_interesse),
            n = n()) %>%
  arrange(desc(n))

# Criação de gráficos
ggplot(dados_limpos, aes(x = coluna_desejada, y = coluna_interesse)) +
  geom_boxplot() +
  labs(title = "Boxplot da coluna de interesse por categoria",
       x = "Categoria",
       y = "Coluna de interesse") +
  theme_bw()

ggplot(dados_limpos, aes(x = coluna_desejada, fill = coluna_desejada)) +
  geom_bar() +
  labs(title = "Frequência da coluna de interesse por categoria",
       x = "Categoria",
       y = "Frequência") +
  theme_bw()

# Salvando os resultados
write_csv(resumo_estatistico, "caminho_para_salvar_o_resumo_estatistico.csv")
ggsave("caminho_para_salvar_o_boxplot.png")
ggsave("caminho_para_salvar_o_grafico_de_barras.png")
```

Neste código, assumimos que você já possui um arquivo CSV com os dados que deseja analisar. O código realiza a leitura desse arquivo, faz a limpeza e pré-processamento dos dados, realiza uma análise exploratória dos mesmos, cria dois gráficos (um boxplot e um gráfico de barras) e salva os resultados em arquivos CSV e imagens PNG.

É importante substituir "caminho_para_o_arquivo.csv", "coluna_desejada" e "coluna_interesse" pelos nomes reais do arquivo e das colunas que você deseja utilizar. Além disso, é necessário fornecer o caminho onde deseja salvar os resultados.

Espero que este código atenda às suas necessidades!