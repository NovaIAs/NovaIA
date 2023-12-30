Claro! Aqui está um exemplo de código complexo em R:

```R
# Função para calcular o fatorial de um número
fatorial <- function(n) {
  if (n < 0) {
    stop("O número deve ser não negativo!")
  } else {
    if (n == 0 || n == 1) {
      return(1)
    } else {
      resultado <- 1
      for (i in 1:n) {
        resultado <- resultado * i
      }
      return(resultado)
    }
  }
}

# Função para calcular a média de um vetor
media <- function(vetor) {
  soma <- sum(vetor)
  n <- length(vetor)
  return(soma/n)
}

# Função para calcular o desvio padrão de um vetor
desvio_padrao <- function(vetor) {
  n <- length(vetor)
  media_vetor <- media(vetor)
  soma <- 0
  for (i in 1:n) {
    soma <- soma + (vetor[i] - media_vetor)^2
  }
  return(sqrt(soma/(n-1)))
}

# Função para gerar uma sequência de Fibonacci até um certo limite
fibonacci <- function(limite) {
  sequencia <- c(0, 1)
  while (sequencia[length(sequencia)] + sequencia[length(sequencia)-1] <= limite) {
    sequencia <- c(sequencia, sequencia[length(sequencia)] + sequencia[length(sequencia)-1])
  }
  return(sequencia)
}

# Exemplo de uso das funções
numero <- 5
fat_numero <- fatorial(numero)
print(paste("O fatorial de", numero, "é", fat_numero))

vetor <- c(1, 2, 3, 4, 5)
media_vetor <- media(vetor)
print(paste("A média do vetor", vetor, "é", media_vetor))

desvio_padrao_vetor <- desvio_padrao(vetor)
print(paste("O desvio padrão do vetor", vetor, "é", desvio_padrao_vetor))

limite <- 100
sequencia_fibonacci <- fibonacci(limite)
print(paste("A sequência de Fibonacci até o limite", limite, "é", sequencia_fibonacci))
```

Neste exemplo de código em R, são definidas quatro funções: `fatorial`, `media`, `desvio_padrao` e `fibonacci`. A função `fatorial` calcula o fatorial de um número não negativo. A função `media` calcula a média de um vetor de números. A função `desvio_padrao` calcula o desvio padrão de um vetor de números. A função `fibonacci` gera uma sequência de Fibonacci até um certo limite.

Além das funções, o código também mostra exemplos de uso das funções, calculando o fatorial de um número, a média e o desvio padrão de um vetor e gerando uma sequência de Fibonacci até um limite específico.