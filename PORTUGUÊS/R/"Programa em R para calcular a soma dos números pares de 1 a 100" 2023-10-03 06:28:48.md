Claro, vou gerar um código complexo em R para você. Aqui está um exemplo de código que usa loops, condicionais e funções para calcular a soma dos números pares de uma lista de 1 a 100:

```R
# Função para verificar se um número é par
is_par <- function(num) {
  if (num %% 2 == 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Gerar uma lista de 1 a 100
lista <- 1:100

# Inicializar a variável para armazenar a soma dos números pares
soma_pares <- 0

# Loop para percorrer a lista e somar os números pares
for (num in lista) {
  if (is_par(num)) {
    soma_pares <- soma_pares + num
  }
}

# Imprimir a soma dos números pares
print(paste("A soma dos números pares de 1 a 100 é:", soma_pares))
```

Neste código, primeiro definimos uma função chamada `is_par`, que recebe um número como argumento e verifica se ele é par usando o operador módulo (`%%`). Se o número for divisível por 2 (ou seja, o resto da divisão por 2 é 0), a função retorna `TRUE`, caso contrário retorna `FALSE`.

Em seguida, geramos uma lista de 1 a 100 usando o operador `:`. Em seguida, inicializamos uma variável chamada `soma_pares` para armazenar a soma dos números pares.

Em seguida, usamos um loop `for` para percorrer cada número na lista. Dentro do loop, verificamos se o número é par usando a função `is_par`. Se for par, adicionamos o número à variável `soma_pares`.

Finalmente, imprimimos a soma dos números pares usando a função `print`.

Espero que este código atenda aos seus requisitos de complexidade e seja útil para você!