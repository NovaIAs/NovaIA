Claro! Aqui está um exemplo de código complexo em R:

```R
# Função para calcular a média ponderada
media_ponderada <- function(valores, pesos) {
  soma <- 0
  for (i in 1:length(valores)) {
    soma <- soma + valores[i] * pesos[i]
  }
  media <- soma / sum(pesos)
  return(media)
}

# Gerando valores aleatórios
set.seed(123)
valores <- rnorm(10, mean = 50, sd = 10)
pesos <- runif(10, min = 1, max = 5)

# Calculando a média ponderada
resultado <- media_ponderada(valores, pesos)

# Imprimindo os resultados
cat("Valores:", valores, "\n")
cat("Pesos:", pesos, "\n")
cat("Média ponderada:", resultado, "\n")
```

Neste exemplo, temos uma função chamada `media_ponderada` que recebe dois argumentos: `valores` e `pesos`. Essa função calcula a média ponderada dos valores, multiplicando cada valor pelo seu respectivo peso e dividindo a soma desses produtos pela soma dos pesos.

Em seguida, é utilizado o `set.seed(123)` para garantir a reprodutibilidade dos valores aleatórios gerados. Os valores são gerados usando a função `rnorm`, que gera 10 valores aleatórios seguindo uma distribuição normal com média 50 e desvio padrão 10. Os pesos são gerados usando a função `runif`, que gera 10 valores aleatórios seguindo uma distribuição uniforme entre 1 e 5.

Depois, é chamada a função `media_ponderada` passando os valores e os pesos gerados. O resultado é armazenado na variável `resultado`.

Por fim, são impressos na tela os valores gerados, os pesos e o resultado da média ponderada usando a função `cat`.