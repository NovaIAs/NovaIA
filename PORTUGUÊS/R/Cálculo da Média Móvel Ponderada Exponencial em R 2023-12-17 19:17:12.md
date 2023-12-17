Claro! Aqui está um código complexo em R:

```R
# Função para calcular a média móvel ponderada exponencialmente
media_movel_ponderada_exp <- function(dados, peso = 0.5) {
  n <- length(dados)
  
  # Inicializa o vetor de médias móveis
  medias_moveis <- numeric(n)
  
  # Calcula a primeira média móvel
  medias_moveis[1] <- dados[1]
  
  # Calcula as médias móveis subsequentes
  for (i in 2:n) {
    medias_moveis[i] <- peso * dados[i] + (1 - peso) * medias_moveis[i - 1]
  }
  
  return(medias_moveis)
}

# Gera um vetor de dados aleatórios
dados <- rnorm(100)
  
# Calcula a média móvel ponderada exponencialmente com peso igual a 0.7
medias_moveis <- media_movel_ponderada_exp(dados, peso = 0.7)

# Plota os dados originais e as médias móveis
plot(dados, type = "l", col = "blue", main = "Média Móvel Ponderada Exponencial")
lines(medias_moveis, col = "red", lwd = 2)
legend("topright", legend = c("Dados Originais", "Médias Móveis"), col = c("blue", "red"), lwd = c(1, 2))
```

Neste código, eu criei uma função chamada `media_movel_ponderada_exp`, que calcula a média móvel ponderada exponencialmente de um vetor de dados. A função recebe como parâmetros o vetor de dados e o peso que será utilizado para calcular a média móvel.

Em seguida, eu gerei um vetor de dados aleatórios usando a função `rnorm`, que gera números aleatórios de uma distribuição normal.

Depois, eu chamei a função `media_movel_ponderada_exp`, passando o vetor de dados e um peso igual a 0.7. Os valores resultantes são armazenados na variável `medias_moveis`.

Por fim, eu fiz um gráfico utilizando a função `plot`, onde plotamos os dados originais em azul e as médias móveis em vermelho. A função `legend` é utilizada para adicionar uma legenda ao gráfico.

Esse código calcula a média móvel ponderada exponencialmente de um vetor de dados e plota um gráfico com os dados originais e as médias móveis.