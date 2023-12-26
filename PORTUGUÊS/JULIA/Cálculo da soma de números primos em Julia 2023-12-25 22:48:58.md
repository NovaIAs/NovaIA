Claro! Aqui está um código complexo em Julia que calcula a soma de todos os números primos menores que um determinado número:

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

# Função para calcular a soma de todos os números primos menores que um número
function sum_primes(n)
    sum = 0

    for i in 2:n
        if is_prime(i)
            sum += i
        end
    end

    return sum
end

# Entrada do usuário
println("Digite um número:")
n = parse(Int64, readline())

# Cálculo da soma dos números primos
result = sum_primes(n)

# Saída do resultado
println("A soma de todos os números primos menores que $n é $result.")
```

Explicação do código:

1. Definimos uma função `is_prime()` que verifica se um número é primo ou não. A função itera de 2 até a raiz quadrada do número `n` e verifica se há algum divisor. Se houver, o número não é primo e a função retorna `false`, caso contrário, retorna `true`.

2. Definimos uma função `sum_primes()` que recebe um número `n` e calcula a soma de todos os números primos menores que `n`. A função itera de 2 até `n` e, para cada número, verifica se é primo usando a função `is_prime()`. Se for primo, o número é adicionado à variável `sum`.

3. Solicitamos ao usuário que digite um número e o armazenamos na variável `n` após converter para o tipo `Int64`.

4. Chamamos a função `sum_primes(n)` para calcular a soma dos números primos menores que `n` e armazenamos o resultado na variável `result`.

5. Imprimimos a mensagem final, mostrando o número digitado pelo usuário e a soma dos números primos menores que esse número.

Espero que este código complexo em Julia atenda às suas expectativas! Se tiver mais alguma dúvida ou necessitar de outros códigos, estou à disposição.