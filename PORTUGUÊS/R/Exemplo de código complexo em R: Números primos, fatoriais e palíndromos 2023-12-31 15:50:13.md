Claro! Aqui está um exemplo de código complexo em R:

```R
# Código complexo em R

# Função para gerar números primos
gerar_primos <- function(n) {
  primos <- c(2)
  num <- 3
  
  while(length(primos) < n) {
    if(all(num %% primos != 0)) {
      primos <- c(primos, num)
    }
    num <- num + 2
  }
  
  return(primos)
}

# Função para calcular o fatorial de um número
fatorial <- function(n) {
  if(n <= 1) {
    return(1)
  } else {
    return(n * fatorial(n-1))
  }
}

# Função para verificar se um número é palíndromo
eh_palindromo <- function(n) {
  n_str <- as.character(n)
  n_rev <- rev(n_str)
  return(n_str == n_rev)
}

# Função principal para encontrar números interessantes
encontrar_numeros_interessantes <- function() {
  primos <- gerar_primos(100)
  fatoriais <- c()
  palindromos <- c()
  
  for(i in 1:100) {
    fatoriais <- c(fatoriais, fatorial(i))
    if(eh_palindromo(i)) {
      palindromos <- c(palindromos, i)
    }
  }
  
  return(list(primos = primos, fatoriais = fatoriais, palindromos = palindromos))
}

# Chamar a função principal e imprimir os resultados
resultado <- encontrar_numeros_interessantes()
print(resultado$primos)
print(resultado$fatoriais)
print(resultado$palindromos)
```

Neste código, temos três funções diferentes:

1. A função `gerar_primos(n)` que recebe um número `n` como parâmetro e gera uma lista com os `n` primeiros números primos.
2. A função `fatorial(n)` que calcula o fatorial de um número `n`, utilizando recursão.
3. A função `eh_palindromo(n)` que verifica se um número `n` é um palíndromo, ou seja, se ele é igual ao seu reverso.

A função principal `encontrar_numeros_interessantes()` utiliza as três funções acima para gerar uma lista de números interessantes, que inclui os 100 primeiros números primos, os fatoriais dos números de 1 a 100 e os números palíndromos de 1 a 100.

Por fim, o código chama a função principal, armazena o resultado em uma variável `resultado` e imprime os três conjuntos de números interessantes.