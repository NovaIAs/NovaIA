Claro! Aqui está um código complexo em R:

```
# Código complexo em R - NovaIA

# Definindo uma função para calcular o fatorial de um número
fatorial <- function(n) {
  if (n == 0 || n == 1) {
    return(1)
  } else {
    return(n * fatorial(n-1))
  }
}

# Criando uma lista vazia
lista_numeros <- list()

# Preenchendo a lista com números de 1 a 10
for (i in 1:10) {
  lista_numeros[[i]] <- i
}

# Criando uma matriz vazia de 5 linhas e 5 colunas
matriz <- matrix(nrow = 5, ncol = 5)

# Preenchendo a matriz com valores aleatórios de 1 a 100
for (i in 1:5) {
  for (j in 1:5) {
    matriz[i, j] <- sample(1:100, 1)
  }
}

# Calculando a média dos valores da matriz
media <- mean(matriz)

# Criando um vetor com os números primos de 1 a 100
vetor_primos <- c()
for (i in 1:100) {
  if (all(i %% 2:(i-1) != 0)) {
    vetor_primos <- c(vetor_primos, i)
  }
}

# Imprimindo na tela o fatorial de cada número da lista
for (i in 1:length(lista_numeros)) {
  cat("O fatorial de", lista_numeros[[i]], "é", fatorial(lista_numeros[[i]]), "\n")
}

# Imprimindo a matriz
print("Matriz:")
print(matriz)

# Imprimindo a média dos valores da matriz
cat("A média dos valores da matriz é", media, "\n")

# Imprimindo o vetor de números primos
cat("Vetor de números primos:")
print(vetor_primos)
```

Este código realiza várias tarefas complexas em R. Primeiro, definimos uma função para calcular o fatorial de um número. Em seguida, criamos uma lista vazia e a preenchemos com números de 1 a 10. Depois, criamos uma matriz vazia de 5x5 e a preenchemos com valores aleatórios de 1 a 100.

Em seguida, calculamos a média dos valores da matriz e criamos um vetor com os números primos de 1 a 100. Por fim, imprimimos na tela o fatorial de cada número da lista, a matriz, a média dos valores da matriz e o vetor de números primos.

Espero que este código complexo seja útil para você! Se precisar de mais alguma coisa, é só me pedir.