Claro! Aqui está um exemplo de código em TCL que utiliza funções e loops para realizar cálculos matemáticos:

```tcl
# Função para calcular o fatorial de um número
proc fatorial {n} {
    set resultado 1
    for {set i 1} {$i <= $n} {incr i} {
        set resultado [expr $resultado * $i]
    }
    return $resultado
}

# Função para calcular o número de combinações possíveis (nCr)
proc combinacoes {n r} {
    set numerador [fatorial $n]
    set denominador [expr [fatorial $r] * [fatorial [expr $n - $r]]]
    set resultado [expr $numerador / $denominador]
    return $resultado
}

# Função para calcular o número de permutações possíveis (nPr)
proc permutacoes {n r} {
    set resultado [expr [fatorial $n] / [fatorial [expr $n - $r]]]
    return $resultado
}

# Exemplo de uso das funções
set numero 5
set escolha 2

set combinacoes_resultado [combinacoes $numero $escolha]
set permutacoes_resultado [permutacoes $numero $escolha]

puts "O número de combinações possíveis de $numero escolhendo $escolha é: $combinacoes_resultado"
puts "O número de permutações possíveis de $numero escolhendo $escolha é: $permutacoes_resultado"
```

Neste código, temos três funções principais: `fatorial`, `combinacoes` e `permutacoes`. A função `fatorial` calcula o fatorial de um número fornecido, utilizando um loop `for` para multiplicar todos os números de 1 até o número fornecido. A função `combinacoes` calcula o número de combinações possíveis utilizando a fórmula nCr, que envolve o cálculo do fatorial. A função `permutacoes` calcula o número de permutações possíveis utilizando a fórmula nPr, também envolvendo o cálculo do fatorial.

No exemplo de uso das funções, definimos o valor de `numero` como 5 e `escolha` como 2. Em seguida, chamamos as funções `combinacoes` e `permutacoes` passando os valores de `numero` e `escolha` como argumentos. Os resultados são armazenados nas variáveis `combinacoes_resultado` e `permutacoes_resultado`. Por fim, utilizamos o comando `puts` para exibir os resultados na saída do console.

Este código demonstra como usar funções e loops em TCL para realizar cálculos matemáticos. Note que as funções `combinacoes` e `permutacoes` chamam a função `fatorial` para realizar os cálculos necessários.