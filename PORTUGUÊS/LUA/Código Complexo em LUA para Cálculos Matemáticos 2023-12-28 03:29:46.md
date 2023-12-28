Claro! Aqui está um exemplo de código complexo em LUA:

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
function isPrimo(num)
    if num <= 1 then
        return false
    end

    for i = 2, math.sqrt(num) do
        if num % i == 0 then
            return false
        end
    end

    return true
end

-- Função para calcular a sequência de Fibonacci
function fibonacci(n)
    local fib = {0, 1}
    for i = 3, n do
        fib[i] = fib[i - 1] + fib[i - 2]
    end
    return fib
end

-- Função para encontrar o máximo divisor comum de dois números
function mdc(a, b)
    while b ~= 0 do
        a, b = b, a % b
    end
    return a
end

-- Função para converter um número decimal para binário
function decimalParaBinario(decimal)
    local binario = ""
    while decimal > 0 do
        binario = (decimal % 2) .. binario
        decimal = math.floor(decimal / 2)
    end
    return binario
end

-- Função para encontrar o número máximo em um array
function encontrarMaximo(array)
    local maximo = array[1]
    for i = 2, #array do
        if array[i] > maximo then
            maximo = array[i]
        end
    end
    return maximo
end

-- Função principal
function main()
    print("Digite um número para calcular o fatorial:")
    local numero = tonumber(io.read())
    local resultadoFatorial = fatorial(numero)
    print("O fatorial de " .. numero .. " é " .. resultadoFatorial)

    print("Digite um número para verificar se é primo:")
    numero = tonumber(io.read())
    local resultadoPrimo = isPrimo(numero)
    if resultadoPrimo then
        print(numero .. " é um número primo.")
    else
        print(numero .. " não é um número primo.")
    end

    print("Digite um número para calcular a sequência de Fibonacci:")
    numero = tonumber(io.read())
    local resultadoFibonacci = fibonacci(numero)
    print("A sequência de Fibonacci até o " .. numero .. "º termo é:")
    for i = 1, #resultadoFibonacci do
        print(resultadoFibonacci[i])
    end

    print("Digite dois números para calcular o máximo divisor comum:")
    local numero1 = tonumber(io.read())
    local numero2 = tonumber(io.read())
    local resultadoMDC = mdc(numero1, numero2)
    print("O máximo divisor comum de " .. numero1 .. " e " .. numero2 .. " é " .. resultadoMDC)

    print("Digite um número decimal para converter para binário:")
    numero = tonumber(io.read())
    local resultadoBinario = decimalParaBinario(numero)
    print(numero .. " em binário é " .. resultadoBinario)

    print("Digite uma sequência de números separados por espaço para encontrar o máximo:")
    local sequencia = io.read()
    local array = {}
    for n in sequencia:gmatch("%S+") do
        table.insert(array, tonumber(n))
    end
    local resultadoMaximo = encontrarMaximo(array)
    print("O máximo número na sequência é " .. resultadoMaximo)
end

-- Chamada da função principal
main()
```

Neste exemplo, criei diversas funções em LUA para calcular o fatorial de um número, verificar se um número é primo, calcular a sequência de Fibonacci, encontrar o máximo divisor comum de dois números, converter um número decimal para binário e encontrar o número máximo em uma sequência de números. A função principal (main) faz uso dessas funções, solicitando entradas do usuário e exibindo os resultados na tela.

Espero que esse código complexo em LUA atenda às suas expectativas!