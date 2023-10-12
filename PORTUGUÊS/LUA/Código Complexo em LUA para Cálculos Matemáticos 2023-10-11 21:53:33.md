Claro! Aqui está um código complexo em LUA:

```lua
-- Função para calcular o fatorial de um número
function fatorial(n)
    if n == 0 then
        return 1
    else
        return n * fatorial(n - 1)
    end
end

-- Função para verificar se um número é primo
function ehPrimo(n)
    if n <= 1 then
        return false
    end

    for i = 2, math.sqrt(n) do
        if n % i == 0 then
            return false
        end
    end

    return true
end

-- Função para gerar uma sequência de Fibonacci
function fibonacci(n)
    local fib = {0, 1}

    for i = 3, n do
        fib[i] = fib[i - 1] + fib[i - 2]
    end

    return fib
end

-- Função para calcular o somatório de uma sequência de números
function somatorio(n)
    local soma = 0

    for i = 1, n do
        soma = soma + i
    end

    return soma
end

-- Função principal
function main()
    print("Digite um número:")
    local numero = tonumber(io.read())

    print("O fatorial de " .. numero .. " é " .. fatorial(numero))
    print("O número " .. numero .. " é primo? " .. (ehPrimo(numero) and "Sim" or "Não"))
    
    print("A sequência de Fibonacci até " .. numero .. " é:")
    local fib = fibonacci(numero)
    for i = 1, #fib do
        print(fib[i])
    end

    print("O somatório de 1 até " .. numero .. " é " .. somatorio(numero))
end

-- Chamada da função principal
main()
```

Este código em LUA possui várias funções que realizam diferentes cálculos matemáticos:

1. A função `fatorial` calcula o fatorial de um número utilizando recursão.
2. A função `ehPrimo` verifica se um número é primo, verificando se ele é divisível por algum número além de 1 e ele mesmo.
3. A função `fibonacci` gera uma sequência de Fibonacci até o número fornecido como entrada.
4. A função `somatorio` calcula o somatório de todos os números de 1 até o número fornecido como entrada.

A função `main` é a função principal que interage com o usuário, solicitando um número e exibindo os resultados dos cálculos utilizando as funções previamente definidas.

Esse código é bastante complexo e realiza diferentes cálculos matemáticos, demonstrando a versatilidade e capacidade da linguagem LUA.