Claro, vou gerar um código complexo em R para você. Este código simula uma rede neural artificial profunda que classifica imagens usando o conjunto de dados MNIST.

```R
# Bibliotecas necessárias
library(neuralnet)
library(reshape2)
library(ggplot2)

# Carregar o conjunto de dados MNIST
dados <- read.table("mnist.csv", header = TRUE, sep = ",")
dados <- data.frame(lapply(dados, as.numeric))

# Dividir o conjunto de dados em treinamento e teste
amostra <- sample(1:nrow(dados), nrow(dados)*0.8)
treinamento <- dados[amostra, ]
teste <- dados[-amostra, ]

# Pré-processamento dos dados
treinamento$x <- treinamento$x / 255
teste$x <- teste$x / 255

# Transformar as variáveis de saída em formato de matriz
treinamento$y <- model.matrix(~y+0, data = treinamento[, "y"])
teste$y <- model.matrix(~y+0, data = teste[, "y"])

# Criar a estrutura da rede neural
rede_neural <- neuralnet(
  formula = y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18 + x19 + x20 + x21 + x22 + x23 + x24 + x25 + x26 + x27 + x28,
  data = treinamento,
  hidden = c(10, 5),
  linear.output = FALSE
)

# Treinar a rede neural
treinamento_rede <- train(
  rede_neural,
  treinamento,
  learningrate = 0.01,
  algorithm = "backprop",
  err.fct = "ce",
  act.fct = "logistic",
  hidden = c(10, 5),
  rep = 10,
  maxit = 1000
)

# Fazer previsões com a rede neural treinada
previsoes <- compute(rede_neural, teste$x)
previsoes <- previsoes$net.result

# Avaliar a acurácia da rede neural
acuracia <- sum(max.col(previsoes) == max.col(teste$y)) / nrow(teste) * 100
print(paste("A acurácia da rede neural é:", acuracia, "%"))

# Plotar gráfico da função de custo ao longo das iterações
custo <- treinamento_rede$algorithm$criterion
iteracoes <- treinamento_rede$algorithm$it
dados_grafico <- data.frame(Iteração = iteracoes, Função_de_Custo = custo)
ggplot(data = dados_grafico, aes(x = Iteração, y = Função_de_Custo)) +
  geom_line() +
  xlab("Número de Iterações") +
  ylab("Função de Custo") +
  ggtitle("Gráfico da Função de Custo ao Longo das Iterações")
```

Neste código, começamos carregando o conjunto de dados MNIST, que contém imagens de dígitos escritos à mão. Em seguida, dividimos o conjunto de dados em treinamento e teste. Os dados são pré-processados, normalizando as imagens para que os valores estejam entre 0 e 1.

Em seguida, criamos a estrutura da rede neural usando a função `neuralnet`. Definimos um modelo com uma camada oculta de 10 neurônios e uma segunda camada oculta de 5 neurônios. A função de ativação usada é a função logística.

Treinamos a rede neural usando o algoritmo de retropropagação (`backprop`) e a função de erro `ce` (entropia cruzada). Especificamos uma taxa de aprendizado de 0.01 e um número máximo de iterações de 1000.

Após o treinamento, fazemos previsões com a rede neural treinada e calculamos a acurácia das previsões em relação aos rótulos reais dos dados de teste.

Por fim, plotamos um gráfico da função de custo ao longo das iterações de treinamento usando a biblioteca `ggplot2`.

Certifique-se de ter o conjunto de dados MNIST salvo como um arquivo CSV chamado "mnist.csv" no mesmo diretório onde o código está sendo executado.