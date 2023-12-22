Claro! Aqui está um código complexo em R:

```R
# Função para calcular a média de uma matriz
calcular_media <- function(matriz) {
  soma <- 0
  elementos <- 0
  
  for (i in 1:nrow(matriz)) {
    for (j in 1:ncol(matriz)) {
      soma <- soma + matriz[i, j]
      elementos <- elementos + 1
    }
  }
  
  media <- soma / elementos
  return(media)
}

# Função para calcular o desvio padrão de uma matriz
calcular_desvio_padrao <- function(matriz) {
  media <- calcular_media(matriz)
  soma_diferencas <- 0
  elementos <- 0
  
  for (i in 1:nrow(matriz)) {
    for (j in 1:ncol(matriz)) {
      soma_diferencas <- soma_diferencas + (matriz[i, j] - media)^2
      elementos <- elementos + 1
    }
  }
  
  desvio_padrao <- sqrt(soma_diferencas / (elementos - 1))
  return(desvio_padrao)
}

# Cria uma matriz de números aleatórios
matriz_aleatoria <- matrix(rnorm(100), nrow = 10, ncol = 10)

# Calcula a média da matriz
media_matriz <- calcular_media(matriz_aleatoria)
cat("A média da matriz é:", media_matriz, "\n")

# Calcula o desvio padrão da matriz
desvio_padrao_matriz <- calcular_desvio_padrao(matriz_aleatoria)
cat("O desvio padrão da matriz é:", desvio_padrao_matriz, "\n")
```

Explicação do código:

1. Definimos a função `calcular_media` que recebe uma matriz como parâmetro e calcula a média dos elementos dessa matriz. Utilizamos dois loops for para percorrer cada elemento da matriz, somamos os valores e contamos o número de elementos. No final, dividimos a soma pelo número de elementos para obter a média.

2. Definimos a função `calcular_desvio_padrao` que recebe uma matriz como parâmetro e calcula o desvio padrão dos elementos dessa matriz. Chamamos a função `calcular_media` para obter a média da matriz. Utilizamos dois loops for para percorrer cada elemento da matriz, calculamos a diferença entre o elemento e a média ao quadrado e somamos essas diferenças. Contamos o número de elementos e subtraímos 1 para obter o número de graus de liberdade. No final, dividimos a soma das diferenças ao quadrado pelo número de graus de liberdade e tiramos a raiz quadrada para obter o desvio padrão.

3. Criamos uma matriz aleatória de números utilizando a função `rnorm` do R. Essa função gera números aleatórios seguindo a distribuição normal.

4. Chamamos a função `calcular_media` passando a matriz aleatória como parâmetro e armazenamos o resultado na variável `media_matriz`. Imprimimos o resultado utilizando a função `cat`.

5. Chamamos a função `calcular_desvio_padrao` passando a matriz aleatória como parâmetro e armazenamos o resultado na variável `desvio_padrao_matriz`. Imprimimos o resultado utilizando a função `cat`.

Espero que esse código atenda às suas expectativas! Caso tenha mais alguma dúvida ou necessite de mais ajuda, estou à disposição.