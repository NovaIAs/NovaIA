Claro! Aqui está um código complexo em R que utiliza diversos conceitos e funcionalidades da linguagem:

```
# Função para calcular o fatorial de um número
fatorial <- function(n) {
  if (n <= 1)
    return(1)
  else
    return(n * fatorial(n - 1))
}

# Função para verificar se um número é primo
eh_primo <- function(n) {
  if (n <= 1)
    return(FALSE)
  else if (n <= 3)
    return(TRUE)
  else if (n %% 2 == 0 | n %% 3 == 0)
    return(FALSE)
  else {
    i <- 5
    while (i * i <= n) {
      if (n %% i == 0 | n %% (i + 2) == 0)
        return(FALSE)
      i <- i + 6
    }
    return(TRUE)
  }
}

# Cálculo do somatório dos fatoriais dos números primos de 1 a 100
soma_fatoriais_primos <- function() {
  soma <- 0
  for (i in 1:100) {
    if (eh_primo(i)) {
      soma <- soma + fatorial(i)
    }
  }
  return(soma)
}

# Execução do cálculo e exibição do resultado
resultado <- soma_fatoriais_primos()
print(paste("O resultado do somatório dos fatoriais dos números primos de 1 a 100 é:", resultado))
```

Neste código, temos três funções principais: `fatorial`, `eh_primo` e `soma_fatoriais_primos`.

A função `fatorial` recebe um número inteiro `n` e retorna o fatorial desse número. Ela utiliza uma estrutura de controle `if-else` para verificar se o número é menor ou igual a 1, caso seja, retorna 1. Caso contrário, retorna o produto do número `n` pelo fatorial de `n-1`.

A função `eh_primo` recebe um número inteiro `n` e verifica se ele é primo. Ela utiliza uma estrutura de controle `if-else` para verificar se o número é menor ou igual a 1, menor ou igual a 3, ou se é divisível por 2 ou 3. Caso alguma dessas condições seja verdadeira, retorna `FALSE`. Caso contrário, utiliza uma estrutura de repetição `while` para verificar se o número é divisível por algum número ímpar maior que 3 e menor ou igual à raiz quadrada de `n`. Se for divisível, retorna `FALSE`. Caso contrário, retorna `TRUE`.

A função `soma_fatoriais_primos` realiza o cálculo do somatório dos fatoriais dos números primos de 1 a 100. Ela inicializa uma variável `soma` com zero e utiliza um loop `for` para percorrer os números de 1 a 100. Para cada número, verifica se é primo utilizando a função `eh_primo` e, se for, calcula o fatorial utilizando a função `fatorial` e adiciona o resultado à variável `soma`. Ao final do loop, retorna o valor da variável `soma`.

Por fim, a execução do código chama a função `soma_fatoriais_primos` e armazena o resultado na variável `resultado`. Em seguida, imprime o resultado na tela utilizando a função `print` e a função `paste` para concatenar a mensagem com o valor calculado.

Espero que este código complexo em R atenda às suas expectativas!