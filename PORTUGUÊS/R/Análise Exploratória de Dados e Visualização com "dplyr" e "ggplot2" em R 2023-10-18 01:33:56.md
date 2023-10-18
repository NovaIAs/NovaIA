Claro! Aqui está um código complexo em R que utiliza a biblioteca "dplyr" para manipulação de dados e a biblioteca "ggplot2" para visualização:

```R
# Carregar as bibliotecas
library(dplyr)
library(ggplot2)

# Ler o arquivo de dados
dados <- read.csv("dados.csv")

# Pré-processamento dos dados
dados_preprocessados <- dados %>%
  filter(idade >= 18) %>%
  mutate(imc = peso / (altura^2))

# Análise exploratória dos dados
resumo <- dados_preprocessados %>%
  group_by(genero) %>%
  summarise(media_idade = mean(idade),
            media_peso = mean(peso),
            media_altura = mean(altura),
            media_imc = mean(imc))

# Visualização dos dados
grafico <- ggplot(data = dados_preprocessados, aes(x = idade, y = imc, color = genero)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Idade", y = "IMC", color = "Gênero") +
  theme_minimal()

# Salvar os resultados em arquivos
write.csv(resumo, "resumo.csv")
ggsave("grafico.png", plot = grafico, width = 6, height = 4, dpi = 300)
```

Neste código, iniciamos carregando as bibliotecas "dplyr" e "ggplot2". Em seguida, é feita a leitura de um arquivo de dados chamado "dados.csv". 

Realizamos um pré-processamento dos dados, filtrando apenas os registros em que a idade é maior ou igual a 18 e calculando o índice de massa corporal (IMC) com base no peso e altura. 

A próxima etapa é uma análise exploratória dos dados, agrupando os registros por gênero e calculando a média da idade, peso, altura e IMC para cada grupo. 

Por fim, utilizamos a biblioteca "ggplot2" para criar um gráfico de dispersão com linha de regressão, onde a idade é representada no eixo x, o IMC no eixo y e as cores representam o gênero. 

Os resultados são salvos em dois arquivos: "resumo.csv" contendo as médias calculadas e "grafico.png" com a imagem do gráfico gerado.