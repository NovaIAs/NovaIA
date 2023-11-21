Claro, vou gerar um código complexo na linguagem R para você. Aqui está:

```R
# Código Complexo em R

# Define uma função para verificar a paridade de um número
verificar_paridade <- function(numero) {
  if (numero %% 2 == 0) {
    return("par")
  } else {
    return("ímpar")
  }
}

# Cria uma lista de números de 1 a 100
numeros <- 1:100

# Utiliza a função verificar_paridade para determinar a paridade de cada número
paridade <- sapply(numeros, verificar_paridade)

# Calcula a média dos números pares e ímpares
media_par <- mean(numeros[paridade == "par"])
media_impar <- mean(numeros[paridade == "ímpar"])

# Imprime os resultados
cat("A média dos números pares é:", media_par, "\n")
cat("A média dos números ímpares é:", media_impar, "\n")
```

Neste código, é definida uma função chamada `verificar_paridade`, que recebe um número como argumento e retorna se ele é par ou ímpar. Em seguida, uma lista de números de 1 a 100 é criada. Utilizando a função `sapply`, a função `verificar_paridade` é aplicada a cada elemento da lista, resultando em um vetor chamado `paridade`, que contém a informação de paridade para cada número. Em seguida, a média dos números pares e dos números ímpares é calculada utilizando a função `mean` e armazenada nas variáveis `media_par` e `media_impar`, respectivamente. Por fim, os resultados são impressos utilizando a função `cat`.