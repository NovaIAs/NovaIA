Claro! Aqui está um código complexo em Julia que calcula a sequência de Fibonacci de forma recursiva:

```julia
function fibonacci(n)
    if n <= 0
        error("O número deve ser maior que zero!")
    elseif n == 1
        return 0
    elseif n == 2
        return 1
    else
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

function main()
    println("Digite um número inteiro positivo:")
    n = parse(Int, readline())
    
    println("A sequência de Fibonacci até o número ", n, " é:")
    for i = 1:n
        println(fibonacci(i))
    end
end

main()
```

Explicação do código:

1. Definimos uma função `fibonacci(n)` que calcula o n-ésimo número da sequência de Fibonacci.
2. Dentro da função `fibonacci(n)`, temos uma estrutura de controle `if-elseif-else` para tratar casos especiais:
   - Se o `n` for menor ou igual a zero, lançamos um erro informando que o número deve ser maior que zero.
   - Se `n` for igual a 1, retornamos 0, pois o primeiro número da sequência de Fibonacci é sempre 0.
   - Se `n` for igual a 2, retornamos 1, pois o segundo número da sequência de Fibonacci é sempre 1.
3. Caso `n` não seja nenhum dos casos especiais, chamamos recursivamente a função `fibonacci(n-1)` para obter o (n-1)-ésimo número da sequência e `fibonacci(n-2)` para obter o (n-2)-ésimo número da sequência, e retornamos a soma desses dois números.
4. Em seguida, temos a função `main()` que é responsável por interagir com o usuário:
   - Imprimimos uma mensagem solicitando ao usuário que digite um número inteiro positivo.
   - Utilizamos a função `parse(Int, readline())` para ler a entrada do usuário como uma string e convertê-la para um número inteiro.
   - Imprimimos uma mensagem informando que vamos mostrar a sequência de Fibonacci até o número digitado pelo usuário.
   - Utilizamos um loop `for` para iterar de 1 até o número digitado pelo usuário e, em cada iteração, chamamos a função `fibonacci(i)` para imprimir o i-ésimo número da sequência.

Espero que esse código atenda às suas expectativas!