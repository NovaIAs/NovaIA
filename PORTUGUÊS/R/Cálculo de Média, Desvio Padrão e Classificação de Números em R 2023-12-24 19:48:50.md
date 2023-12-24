Claro! Aqui está um código complexo em R:

```R
# Função para calcular a média de uma lista de números
calcular_media <- function(lista_numeros) {
  soma <- 0
  for (numero in lista_numeros) {
    soma <- soma + numero
  }
  media <- soma / length(lista_numeros)
  return(media)
}

# Função para calcular o desvio padrão de uma lista de números
calcular_desvio_padrao <- function(lista_numeros) {
  media <- calcular_media(lista_numeros)
  soma_diferencas_quadrado <- 0
  for (numero in lista_numeros) {
    diferenca <- numero - media
    soma_diferencas_quadrado <- soma_diferencas_quadrado + diferenca^2
  }
  desvio_padrao <- sqrt(soma_diferencas_quadrado / length(lista_numeros))
  return(desvio_padrao)
}

# Função para classificar uma lista de números em intervalos
classificar_numeros <- function(lista_numeros, intervalos) {
  classificacao <- rep(0, length(lista_numeros))
  for (i in 1:length(intervalos)) {
    intervalo <- intervalos[i]
    for (j in 1:length(lista_numeros)) {
      if (lista_numeros[j] >= intervalo[1] & lista_numeros[j] <= intervalo[2]) {
        classificacao[j] <- i
      }
    }
  }
  return(classificacao)
}

# Exemplo de uso das funções
numeros <- c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
media <- calcular_media(numeros)
desvio_padrao <- calcular_desvio_padrao(numeros)
intervalos <- list(c(0, 10), c(11, 20), c(21, 30), c(31, 40), c(41, 50))
classificacao <- classificar_numeros(numeros, intervalos)

print("Lista de números:")
print(numeros)
print("Média:")
print(media)
print("Desvio Padrão:")
print(desvio_padrao)
print("Classificação:")
print(classificacao)
```

Neste código, são definidas três funções em R: `calcular_media`, `calcular_desvio_padrao` e `classificar_numeros`. 

A função `calcular_media` recebe uma lista de números e retorna a média dos mesmos. Ela utiliza um loop `for` para somar todos os elementos da lista e, em seguida, divide a soma pelo número de elementos para obter a média.

A função `calcular_desvio_padrao` recebe uma lista de números e retorna o desvio padrão. Ela utiliza a função `calcular_media` para obter a média da lista e, em seguida, calcula a soma dos quadrados das diferenças entre cada elemento da lista e a média. Por fim, divide essa soma pelo número de elementos da lista e tira a raiz quadrada para obter o desvio padrão.

A função `classificar_numeros` recebe uma lista de números e uma lista de intervalos e retorna uma lista com a classificação de cada número de acordo com os intervalos. Ela utiliza dois loops `for` para percorrer os intervalos e os números, verificando se cada número está contido em algum intervalo. Se estiver, atribui a classificação correspondente a esse número.

Por fim, um exemplo de uso das funções é apresentado, onde uma lista de números é definida e as funções são utilizadas para calcular a média, o desvio padrão e classificar os números em intervalos. Os resultados são impressos na tela.