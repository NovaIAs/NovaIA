Claro! Aqui está um código complexo em Julia que envolve o uso de funções, loops e estruturas condicionais:

```julia
# Função para verificar se um número é primo
function primo(n)
    if n < 2
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
function fatorial(n)
    if n < 0
        error("O fatorial não está definido para números negativos.")
    end
    if n == 0 || n == 1
        return 1
    end
    return n * fatorial(n - 1)
end

# Função para inverter uma string
function inverter_string(str)
    inv_str = ""
    for i = length(str):-1:1
        inv_str *= str[i]
    end
    return inv_str
end

# Função que retorna a soma dos números primos até um dado limite
function soma_primos(limite)
    soma = 0
    for i in 2:limite
        if primo(i)
            soma += i
        end
    end
    return soma
end

# Função para calcular o número de Fibonacci de um dado termo
function fibonacci(n)
    if n < 0
        error("O termo da sequência de Fibonacci não pode ser negativo.")
    end
    if n == 0 || n == 1
        return n
    end
    return fibonacci(n - 1) + fibonacci(n - 2)
end

# Exemplo de uso das funções
limite = 100

println("A soma dos números primos até $limite é ", soma_primos(limite))

n = 10
println("O fatorial de $n é ", fatorial(n))

str = "Olá, mundo!"
println("A string invertida é '", inverter_string(str), "'")

termo = 6
println("O $termoº termo da sequência de Fibonacci é ", fibonacci(termo))
```

Neste código em Julia, temos várias funções que realizam diferentes cálculos e operações. A função `primo(n)` verifica se um número é primo, usando um loop `for` para testar divisibilidade. A função `fatorial(n)` calcula o fatorial de um número, usando recursão. A função `inverter_string(str)` inverte uma string, percorrendo-a de trás para frente e concatenando os caracteres. A função `soma_primos(limite)` calcula a soma de todos os números primos até um determinado limite. Por fim, a função `fibonacci(n)` calcula o n-ésimo termo da sequência de Fibonacci, também usando recursão.

Após a definição das funções, há um exemplo de uso de cada uma delas. O código imprime a soma dos números primos até um limite especificado, o fatorial de um número, a string invertida e o n-ésimo termo da sequência de Fibonacci.