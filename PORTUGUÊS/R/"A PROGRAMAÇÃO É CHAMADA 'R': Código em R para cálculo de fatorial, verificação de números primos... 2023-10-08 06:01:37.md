Claro! Aqui está um exemplo de um código complexo em R:

```R
# Função para calcular o fatorial de um número
fatorial <- function(n) {
  if (n == 0 || n == 1) {
    return(1)
  } else {
    return(n * fatorial(n - 1))
  }
}

# Função para verificar se um número é primo
isPrimo <- function(n) {
  if (n <= 1) {
    return(FALSE)
  }
  for (i in 2:sqrt(n)) {
    if (n %% i == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

# Função para gerar uma sequência de Fibonacci
fibonacci <- function(n) {
  seq <- c(0, 1)
  if (n <= 2) {
    return(seq[1:n])
  }
  for (i in 3:n) {
    seq[i] <- seq[i-1] + seq[i-2]
  }
  return(seq)
}

# Função principal
main <- function() {
  numero <- as.integer(readline("Digite um número inteiro: "))
  
  resultado_fatorial <- fatorial(numero)
  resultado_fibonacci <- fibonacci(numero)
  resultado_primo <- isPrimo(numero)
  
  cat("O fatorial de", numero, "é:", resultado_fatorial, "\n")
  cat("Os primeiros", numero, "números da sequência de Fibonacci são:", resultado_fibonacci, "\n")
  if (resultado_primo) {
    cat(numero, "é um número primo.\n")
  } else {
    cat(numero, "não é um número primo.\n")
  }
}

# Chamando a função principal
main()
```

Neste código, temos três funções diferentes:

1. `fatorial`: Essa função calcula o fatorial de um número usando uma recursão básica. Ela verifica se o número é 0 ou 1 e retorna 1 nesses casos. Caso contrário, ela chama a si mesma com o argumento `n-1` e multiplica o resultado pelo número original.

2. `isPrimo`: Essa função verifica se um número é primo ou não. Ela retorna `FALSE` se o número for menor ou igual a 1. Caso contrário, ela itera de 2 até a raiz quadrada do número e verifica se o número é divisível por algum desses valores. Se for, ela retorna `FALSE`. Caso contrário, ela retorna `TRUE`.

3. `fibonacci`: Essa função gera uma sequência de Fibonacci até o número especificado. Ela cria um vetor inicial com os valores 0 e 1. Se o número especificado for menor ou igual a 2, ela retorna os primeiros `n` elementos desse vetor. Caso contrário, ela itera a partir do terceiro elemento e calcula cada elemento subsequente somando os dois elementos anteriores.

A função principal `main` é responsável por receber um número inteiro do usuário, chamar as três funções e exibir os resultados na tela usando a função `cat`. Ela também verifica se o número é primo ou não e exibe essa informação.