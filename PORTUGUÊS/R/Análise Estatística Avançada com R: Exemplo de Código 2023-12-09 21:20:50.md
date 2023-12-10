Claro! Aqui está um exemplo de código complexo em R que realiza uma análise estatística avançada usando o pacote dplyr e ggplot2:

```
# Carregar pacotes necessários
library(dplyr)
library(ggplot2)

# Carregar o conjunto de dados
dados <- read.csv("caminho/para/o/arquivo.csv")

# Limpar os dados
dados_limpos <- dados %>%
  filter(!is.na(coluna1) & !is.na(coluna2)) %>%
  mutate(coluna3 = coluna1 + coluna2)

# Criar uma nova variável
dados_limpos <- dados_limpos %>%
  mutate(nova_variavel = ifelse(coluna3 > 10, "Maior que 10", "Menor ou igual a 10"))

# Analisar os dados
resumo_dados <- dados_limpos %>%
  group_by(nova_variavel) %>%
  summarise(media_coluna1 = mean(coluna1),
            media_coluna2 = mean(coluna2),
            desvio_padrao_coluna3 = sd(coluna3))

# Plotar os resultados
grafico <- ggplot(resumo_dados, aes(x = nova_variavel)) +
  geom_bar(aes(y = media_coluna1), fill = "blue", stat = "identity") +
  geom_errorbar(aes(ymin = media_coluna1 - desvio_padrao_coluna3,
                    ymax = media_coluna1 + desvio_padrao_coluna3),
                width = 0.2, color = "red") +
  labs(title = "Análise Estatística Avançada",
       x = "Nova Variável",
       y = "Média Coluna 1") +
  theme_minimal()

# Exibir o gráfico
print(grafico)
```

Neste código, inicialmente carregamos os pacotes dplyr e ggplot2. Em seguida, carregamos um conjunto de dados através da função read.csv(). Aplicamos uma limpeza nos dados, removendo linhas com valores ausentes nas colunas 1 e 2, e criamos uma nova coluna somando as colunas 1 e 2.

Posteriormente, criamos uma nova variável baseada em uma condição, classificando os valores da coluna 3 como "Maior que 10" ou "Menor ou igual a 10". Em seguida, realizamos uma análise estatística agrupando os dados pela nova variável e calculando a média da coluna 1 e 2, bem como o desvio padrão da coluna 3.

Por fim, plotamos os resultados em um gráfico de barras com barras representando a média da coluna 1, e adicionamos barras de erro representando o desvio padrão da coluna 3. O gráfico é personalizado com títulos e tema minimalista.

Espero que esse código atenda às suas expectativas! Lembre-se de substituir "caminho/para/o/arquivo.csv" pelo caminho correto para o seu conjunto de dados.