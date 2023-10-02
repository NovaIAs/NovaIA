Claro! Aqui está um exemplo de código complexo em LUA:

```lua
-- Função para verificar se um número é primo
local function isPrime(num)
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

-- Função para imprimir os números primos até um determinado limite
local function printPrimes(limit)
    for i = 2, limit do
        if isPrime(i) then
            print(i)
        end
    end
end

-- Função recursiva para calcular o fatorial de um número
local function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n - 1)
    end
end

-- Função para calcular a média de uma lista de números
local function calculateAverage(numbers)
    local sum = 0
    for _, num in ipairs(numbers) do
        sum = sum + num
    end
    return sum / #numbers
end

-- Exemplo de uso das funções
local primesLimit = 100
print("Números primos até " .. primesLimit .. ":")
printPrimes(primesLimit)

local number = 5
print("O fatorial de " .. number .. " é " .. factorial(number))

local numList = {10, 20, 30, 40, 50}
print("A média da lista de números é " .. calculateAverage(numList))
```

Este código apresenta algumas funcionalidades interessantes, como a verificação de números primos, o cálculo do fatorial de um número, e o cálculo da média de uma lista de números.