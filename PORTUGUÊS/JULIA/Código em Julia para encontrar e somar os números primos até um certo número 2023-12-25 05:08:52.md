Claro! Aqui está um código complexo em Julia:

```julia
# Função para verificar se um número é primo
function is_prime(n)
    if n < 2
        return false
    end
    
    for i in 2:sqrt(n)
        if n % i == 0
            return false
        end
    end
    
    return true
end

# Função para encontrar os "n" primeiros números primos
function find_primes(n)
    primes = []
    count = 0
    number = 2
    
    while count < n
        if is_prime(number)
            push!(primes, number)
            count += 1
        end
        number += 1
    end
    
    return primes
end

# Função para calcular o somatório dos números primos de 1 até "n"
function prime_sum(n)
    primes = find_primes(n)
    sum = 0
    
    for prime in primes
        sum += prime
    end
    
    return sum
end

# Função principal
function main()
    println("Digite um número inteiro positivo:")
    n = parse(Int64, readline())
    
    sum = prime_sum(n)
    
    println("O somatório dos ", n, " primeiros números primos é ", sum)
end

# Chama a função principal
main()
```

Explicação do código:

1. A função `is_prime(n)` recebe um número `n` como entrada e verifica se ele é primo ou não. Ela retorna `true` se for primo e `false` caso contrário.

2. A função `find_primes(n)` recebe um número `n` como entrada e encontra os `n` primeiros números primos. Ela utiliza a função `is_prime(n)` para verificar se cada número é primo e armazena os números primos em um array chamado `primes`.

3. A função `prime_sum(n)` recebe um número `n` como entrada e calcula o somatório dos números primos de 1 até `n`. Ela utiliza a função `find_primes(n)` para encontrar os números primos e, em seguida, itera sobre eles e faz o somatório.

4. A função `main()` é a função principal do programa. Ela solicita que o usuário digite um número inteiro positivo, lê o número digitado, chama a função `prime_sum(n)` para calcular o somatório dos números primos até `n` e exibe o resultado na tela.

5. A função principal `main()` é chamada no final do código para iniciar a execução do programa.

Esse código em Julia solicita ao usuário um número inteiro positivo, calcula o somatório dos primeiros números primos até esse número e exibe o resultado na tela.