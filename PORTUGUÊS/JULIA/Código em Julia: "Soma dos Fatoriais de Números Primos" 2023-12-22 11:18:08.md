Claro! Vou criar um código complexo em Julia para você. Este código vai calcular a soma dos fatoriais de todos os números primos menores que um número dado pelo usuário. 

Aqui está o código:

```julia
# Função para verificar se um número é primo
function is_prime(n)
    if n <= 1
        return false
    end
    for i in 2:isqrt(n)
        if n % i == 0
            return false
        end
    end
    return true
end

# Função para calcular o fatorial de um número
function factorial(n)
    if n == 0 || n == 1
        return 1
    end
    return n * factorial(n-1)
end

# Função para calcular a soma dos fatoriais de todos os números primos menores que um número dado
function sum_of_factorials_of_primes(n)
    sum = 0
    for i in 2:n
        if is_prime(i)
            sum += factorial(i)
        end
    end
    return sum
end

# Solicita ao usuário um número limite
println("Digite um número limite:")
limite = parse(Int64, readline())

# Calcula e exibe a soma dos fatoriais de todos os números primos menores que o número limite
println("A soma dos fatoriais dos números primos menores que $limite é: ", sum_of_factorials_of_primes(limite))
```

Neste código, temos três funções principais:

1. A função `is_prime(n)` verifica se um número `n` é primo. Ela itera de 2 até a raiz quadrada de `n` e verifica se `n` é divisível por algum número nesse intervalo. Se for, o número não é primo e a função retorna `false`. Caso contrário, retorna `true`.

2. A função `factorial(n)` calcula o fatorial de um número `n` utilizando recursão. Se `n` for igual a 0 ou 1, retorna 1. Caso contrário, retorna `n` multiplicado pelo fatorial de `n-1`.

3. A função `sum_of_factorials_of_primes(n)` calcula a soma dos fatoriais de todos os números primos menores que um número `n`. Ela itera de 2 até `n` e verifica se cada número é primo. Se for, calcula o fatorial desse número e adiciona à soma total. No final, retorna a soma.

Após a definição das funções, o código solicita ao usuário um número limite e chama a função `sum_of_factorials_of_primes` passando esse número como argumento. Em seguida, exibe o resultado da soma dos fatoriais dos números primos menores que o número limite.