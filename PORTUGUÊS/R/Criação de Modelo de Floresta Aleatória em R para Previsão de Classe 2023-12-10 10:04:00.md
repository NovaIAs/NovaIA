Claro! A seguir, vou gerar um código complexo em R para você. Este código é um exemplo de como criar um modelo de aprendizado de máquina usando o algoritmo de floresta aleatória. Este modelo será usado para prever a classe de uma determinada amostra de dados.

```R
# Importando bibliotecas
library(randomForest)

# Carregando o conjunto de dados
data <- read.csv("dados.csv")

# Dividindo o conjunto de dados em treinamento e teste
set.seed(123)
train_indices <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Criando o modelo de floresta aleatória
model_rf <- randomForest(classe ~ ., data = train_data, ntree = 100)

# Realizando a previsão no conjunto de teste
predictions <- predict(model_rf, test_data)

# Avaliando a precisão do modelo
accuracy <- sum(predictions == test_data$classe) / length(predictions)
print(paste("Precisão do modelo:", round(accuracy * 100, 2), "%"))

# Gerando a matriz de confusão
confusion_matrix <- table(predictions, test_data$classe)
print("Matriz de Confusão:")
print(confusion_matrix)
```

Neste código, primeiro importamos a biblioteca `randomForest`, que é necessária para criar um modelo de floresta aleatória. Em seguida, carregamos um conjunto de dados a partir de um arquivo chamado "dados.csv".

Em seguida, dividimos o conjunto de dados em um conjunto de treinamento e um conjunto de teste, usando a função `sample` para selecionar aleatoriamente 70% das amostras para treinamento. O restante das amostras será usado para testar o modelo.

Em seguida, criamos o modelo de floresta aleatória usando a função `randomForest`. Neste exemplo, usamos todos os atributos do conjunto de dados como variáveis independentes e a variável "classe" como variável dependente a ser prevista. Definimos também o número de árvores (`ntree`) como 100.

Após criar o modelo, realizamos a previsão no conjunto de teste usando a função `predict`. Em seguida, calculamos a precisão do modelo, comparando as previsões com as classes reais do conjunto de teste.

Por fim, geramos a matriz de confusão para avaliar o desempenho do modelo. A matriz de confusão nos fornece informações sobre os resultados da classificação, mostrando a quantidade de amostras classificadas corretamente e incorretamente para cada classe.

Espero que este exemplo tenha sido útil! Se você tiver mais alguma dúvida ou precisar de ajuda com qualquer outro código, fique à vontade para perguntar.