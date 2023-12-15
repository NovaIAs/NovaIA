Claro! Aqui está um código complexo em R que realiza um algoritmo de classificação utilizando o conjunto de dados "Iris". O código irá treinar um modelo de classificação usando a técnica de K-vizinhos mais próximos (K-Nearest Neighbors) e avaliar sua precisão.

```R
# Carregando o conjunto de dados Iris
iris_data <- iris

# Dividindo o conjunto de dados em treinamento e teste
set.seed(123)
indices_treinamento <- sample(1:nrow(iris_data), 0.7 * nrow(iris_data))
dados_treinamento <- iris_data[indices_treinamento, ]
dados_teste <- iris_data[-indices_treinamento, ]

# Definindo o número de vizinhos a serem considerados
k <- 5

# Função para calcular a distância Euclidiana entre dois pontos
calcular_distancia <- function(ponto1, ponto2) {
  sqrt(sum((ponto1 - ponto2) ^ 2))
}

# Função para classificar uma instância desconhecida baseada nos vizinhos mais próximos
classificar_instancia <- function(instancia_desconhecida, dados_treinamento, k) {
  distancias <- data.frame(distancia = numeric(), classe = character())
  
  # Calculando a distância entre a instância desconhecida e cada ponto de treinamento
  for (i in 1:nrow(dados_treinamento)) {
    distancia <- calcular_distancia(instancia_desconhecida, dados_treinamento[i, 1:4])
    classe <- dados_treinamento[i, 5]
    distancias <- rbind(distancias, data.frame(distancia = distancia, classe = classe))
  }
  
  # Ordenando as distâncias em ordem crescente
  distancias <- distancias[order(distancias$distancia), ]
  
  # Obtendo as k classes mais próximas
  classes_vizinhos <- distancias[1:k, "classe"]
  
  # Retornando a classe mais frequente entre os vizinhos
  classe_final <- names(which.max(table(classes_vizinhos)))
  
  return(classe_final)
}

# Classificando as instâncias de teste
classes_preditas <- sapply(1:nrow(dados_teste), function(i) 
  classificar_instancia(dados_teste[i, 1:4], dados_treinamento, k))

# Calculando a precisão do modelo
precisao <- sum(classes_preditas == dados_teste$Species) / nrow(dados_teste)
print(paste("Precisão do modelo: ", precisao))
```

Neste código, primeiro carregamos o conjunto de dados "Iris" e dividimos em dados de treinamento e teste. Em seguida, definimos o número de vizinhos (k) a serem considerados e implementamos a função `calcular_distancia` para calcular a distância Euclidiana entre dois pontos. 

A função `classificar_instancia` recebe uma instância desconhecida, os dados de treinamento e o número de vizinhos (k) como parâmetros. Ela calcula a distância entre a instância desconhecida e cada ponto de treinamento, ordena as distâncias em ordem crescente e retorna a classe mais frequente entre os k vizinhos mais próximos.

Em seguida, utilizamos a função `sapply` para aplicar a função `classificar_instancia` em cada instância de teste e obter as classes preditas. Por fim, calculamos a precisão do modelo comparando as classes preditas com as classes reais dos dados de teste.

Espero que este código seja útil para você!