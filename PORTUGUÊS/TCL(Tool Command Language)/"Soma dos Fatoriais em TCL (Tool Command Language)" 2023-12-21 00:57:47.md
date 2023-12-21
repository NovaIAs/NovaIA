A seguir, apresento um código em TCL (Tool Command Language) que realiza uma operação matemática complexa chamada "Soma dos Fatoriais".

```
proc fatorial {n} {
    set resultado 1
    for {set i 1} {$i <= $n} {incr i} {
        set resultado [expr {$resultado * $i}]
    }
    return $resultado
}

proc somaFatoriais {n} {
    set soma 0
    for {set i 1} {$i <= $n} {incr i} {
        set soma [expr {$soma + [fatorial $i]}]
    }
    return $soma
}

set numero 5
set resultado [somaFatoriais $numero]
puts "A soma dos fatoriais de 1 até $numero é: $resultado"
```

Neste código, temos duas funções principais: `fatorial` e `somaFatoriais`.

A função `fatorial` recebe um número `n` como parâmetro e calcula o fatorial desse número utilizando um loop `for`. A variável `resultado` é inicializada com 1 e, a cada iteração, o valor de `resultado` é multiplicado pelo valor de `i`. Ao final, o resultado final do fatorial é retornado.

A função `somaFatoriais` recebe um número `n` como parâmetro e calcula a soma dos fatoriais de 1 até `n`. A variável `soma` é inicializada com 0 e, a cada iteração, o valor de `soma` é incrementado com o valor do fatorial de `i`, utilizando a função `fatorial`. Ao final, o resultado final da soma dos fatoriais é retornado.

No exemplo, é atribuído o valor 5 à variável `numero` e chamada a função `somaFatoriais` passando esse valor como argumento. O resultado é então exibido utilizando a função `puts`.

Este código realiza uma operação matemática complexa, onde a soma dos fatoriais de 1 até `n` é calculada. O fatorial de um número é o produto de todos os números inteiros positivos menores ou iguais a ele.