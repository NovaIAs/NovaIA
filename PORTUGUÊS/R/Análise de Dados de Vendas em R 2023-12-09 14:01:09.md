Claro! Aqui está um código complexo em R, que envolve a manipulação de dados, uso de funções personalizadas e visualização de gráficos:

```R
# Programa para análise de dados de vendas

# Importar os dados
dados <- read.csv("dados_vendas.csv", header = TRUE)

# Função para calcular o total de vendas por mês
calcular_total_vendas <- function(dados) {
  total_vendas <- aggregate(dados$valor_venda, by = list(mês = dados$mês), FUN = sum)
  return(total_vendas)
}

# Calcular o total de vendas por mês
total_vendas <- calcular_total_vendas(dados)

# Função para plotar um gráfico de barras do total de vendas por mês
plotar_grafico_vendas <- function(total_vendas) {
  barplot(total_vendas$x, names.arg = total_vendas$mês, xlab = "Mês", ylab = "Total de Vendas",
          main = "Total de Vendas por Mês", col = "blue")
}

# Plotar o gráfico de vendas
plotar_grafico_vendas(total_vendas)

# Função para calcular a média de vendas por produto
calcular_media_vendas_produto <- function(dados) {
  media_vendas_produto <- aggregate(dados$valor_venda, by = list(produto = dados$produto), FUN = mean)
  return(media_vendas_produto)
}

# Calcular a média de vendas por produto
media_vendas_produto <- calcular_media_vendas_produto(dados)

# Imprimir a média de vendas por produto
print("Média de Vendas por Produto:")
print(media_vendas_produto)

# Função para realizar uma análise de regressão linear simples
realizar_regressao_linear <- function(dados) {
  regressao <- lm(dados$valor_venda ~ dados$quantidade_vendas)
  return(regressao)
}

# Realizar a regressão linear
regressao <- realizar_regressao_linear(dados)

# Imprimir os resultados da regressão linear
print("Resultados da Regressão Linear:")
print(summary(regressao))
```

Neste código, assumimos que você possui um arquivo CSV chamado "dados_vendas.csv" contendo informações sobre vendas, como o mês, o valor da venda, o produto e a quantidade de vendas. 

O código começa importando os dados do arquivo CSV para um objeto chamado "dados". Em seguida, define uma função chamada "calcular_total_vendas" que calcula o total de vendas por mês utilizando a função "aggregate". A função retorna um objeto que armazena os totais de vendas por mês.

Depois, o código chama a função "calcular_total_vendas" passando o objeto "dados" como argumento e armazena o resultado na variável "total_vendas". Em seguida, define-se uma função chamada "plotar_grafico_vendas" que utiliza a função "barplot" para criar um gráfico de barras do total de vendas por mês. O gráfico é exibido na tela.

O código continua definindo uma função chamada "calcular_media_vendas_produto" que calcula a média de vendas por produto utilizando a função "aggregate". A função retorna um objeto que armazena as médias de vendas por produto.

Após isso, o código chama a função "calcular_media_vendas_produto" passando o objeto "dados" como argumento e armazena o resultado na variável "media_vendas_produto". Em seguida, imprime-se a média de vendas por produto na tela.

Por fim, o código define uma função chamada "realizar_regressao_linear" que realiza uma análise de regressão linear simples utilizando a função "lm". A função retorna um objeto que armazena os resultados da regressão.

O código chama a função "realizar_regressao_linear" passando o objeto "dados" como argumento e armazena o resultado na variável "regressao". Por fim, imprime-se os resultados da regressão linear na tela.

Este código é apenas um exemplo de um programa complexo em R, e pode ser adaptado e estendido de acordo com as necessidades específicas do projeto.