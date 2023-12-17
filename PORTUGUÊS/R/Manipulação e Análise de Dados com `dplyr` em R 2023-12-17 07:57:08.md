Claro! Aqui está um código complexo em R que utiliza a biblioteca `dplyr` para realizar manipulação de dados e análise estatística.

```R
# Instalar e carregar pacotes
install.packages("dplyr")
library(dplyr)

# Importar o conjunto de dados
dados <- read.csv("caminho/do/arquivo.csv")

# Limpar os dados
dados_limpos <- dados %>%
  filter(!is.na(variavel1) & !is.na(variavel2)) %>%
  select(variavel1, variavel2)

# Criar uma nova variável
dados_nova_variavel <- dados_limpos %>%
  mutate(nova_variavel = variavel1 + variavel2)

# Agrupar os dados por uma variável
dados_agrupados <- dados_nova_variavel %>%
  group_by(variavel3) %>%
  summarize(media = mean(nova_variavel),
            desvio_padrao = sd(nova_variavel),
            total = n())

# Ordenar os dados por uma variável
dados_ordenados <- dados_agrupados %>%
  arrange(desc(media))

# Exibir os primeiros registros
head(dados_ordenados)
```

Neste código, primeiro instalamos e carregamos a biblioteca `dplyr`. Em seguida, importamos o conjunto de dados do arquivo CSV especificado.

Depois de importar os dados, realizamos algumas transformações. Primeiro, removemos linhas com valores ausentes nas colunas `variavel1` e `variavel2`, usando a função `filter` do `dplyr`. Em seguida, selecionamos somente as colunas `variavel1` e `variavel2`, usando a função `select`.

Em seguida, criamos uma nova variável chamada `nova_variavel`, que é a soma das colunas `variavel1` e `variavel2`, usando a função `mutate`.

Agrupamos os dados pela coluna `variavel3`, usando a função `group_by`, e calculamos a média, desvio padrão e o total da `nova_variavel` para cada grupo, usando a função `summarize`.

Por fim, ordenamos os dados pela média em ordem decrescente, usando a função `arrange`, e exibimos os primeiros registros usando a função `head`.

Espero que este código complexo em R atenda às suas expectativas! Lembre-se de substituir "caminho/do/arquivo.csv" pelo caminho real do seu arquivo de dados.