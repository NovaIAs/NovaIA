Claro! Aqui está um exemplo de código complexo em Lua:

```lua
-- função para verificar se um número é primo
local function ehPrimo(numero)
    if numero < 2 then 
        return false
    end
    
    for i = 2, math.sqrt(numero) do
        if numero % i == 0 then
            return false
        end
    end
    
    return true
end

-- função para calcular a soma dos números primos até um determinado limite
local function calcularSomaPrimos(limite)
    local soma = 0
    
    for i = 2, limite do
        if ehPrimo(i) then
            soma = soma + i
        end
    end
    
    return soma
end

-- função para exibir a sequência de Fibonacci até um determinado número de termos
local function exibirFibonacci(numTermos)
    local sequencia = {0, 1}
    
    for i = 3, numTermos do
        sequencia[i] = sequencia[i-1] + sequencia[i-2]
    end
    
    return sequencia
end

-- função para verificar se uma palavra é um palíndromo
local function ehPalindromo(palavra)
    palavra = palavra:lower()
    
    local tamanho = #palavra
    local metade = math.floor(tamanho / 2)
    
    for i = 1, metade do
        if palavra:sub(i, i) ~= palavra:sub(tamanho - i + 1, tamanho - i + 1) then
            return false
        end
    end
    
    return true
end

-- chamando as funções e exibindo os resultados
print("Soma dos números primos até 1000: " .. calcularSomaPrimos(1000))

local fibonacci = exibirFibonacci(10)
print("Sequência de Fibonacci com 10 termos: ")
for i = 1, #fibonacci do
    io.write(fibonacci[i] .. " ")
end
io.write("\n")

print("A palavra 'arara' é um palíndromo? " .. tostring(ehPalindromo("arara")))
```

Neste exemplo, o código possui quatro funções:

1. A função `ehPrimo` verifica se um número é primo ou não.
2. A função `calcularSomaPrimos` calcula a soma de todos os números primos até um determinado limite.
3. A função `exibirFibonacci` exibe a sequência de Fibonacci até um determinado número de termos.
4. A função `ehPalindromo` verifica se uma palavra é um palíndromo, ou seja, se pode ser lida da mesma forma da esquerda para a direita e vice-versa.

No final, as funções são chamadas e os resultados são exibidos no console.