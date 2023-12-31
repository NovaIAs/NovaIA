Claro! Aqui está um exemplo de um código complexo em Julia:

```julia
# Função para verificar se um número é primo
function is_prime(n)
    if n <= 1
        return false
    elseif n <= 3
        return true
    elseif n % 2 == 0 || n % 3 == 0
        return false
    end
    
    i = 5
    while i * i <= n
        if n % i == 0 || n % (i + 2) == 0
            return false
        end
        i += 6
    end
    
    return true
end

# Função para calcular a soma dos números primos de 1 até um dado número
function sum_primes(n)
    sum = 0
    for i in 1:n
        if is_prime(i)
            sum += i
        end
    end
    return sum
end

# Função para calcular o fatorial de um número
function factorial(n)
    if n == 0 || n == 1
        return 1
    else
        return n * factorial(n - 1)
    end
end

# Função principal
function main()
    println("Digite um número:")
    num = parse(Int64, readline())
    
    println("A soma dos números primos de 1 até ", num, " é: ", sum_primes(num))
    
    println("Digite outro número:")
    num2 = parse(Int64, readline())
    
    println("O fatorial de ", num2, " é: ", factorial(num2))
end

# Chamando a função principal
main()
```

Neste código, temos três funções principais:

1. A função `is_prime(n)` verifica se um número `n` é primo, utilizando o algoritmo de verificação de primalidade com base na divisão por números primos conhecidos (2 e 3), seguido por uma iteração até a raiz quadrada do número.

2. A função `sum_primes(n)` calcula a soma de todos os números primos de 1 até um dado número `n`, utilizando a função `is_prime(n)` para verificar se cada número é primo e, em caso afirmativo, adicionando-o à soma.

3. A função `factorial(n)` calcula o fatorial de um número `n` utilizando uma abordagem recursiva, verificando se o número é igual a 0 ou 1 e, caso contrário, multiplicando-o pelo fatorial do número anterior.

A função principal `main()` solicita ao usuário que digite dois números, chama as funções `sum_primes(n)` e `factorial(n)` com esses números e imprime os resultados na tela.

Espero que este código atenda às suas expectativas!