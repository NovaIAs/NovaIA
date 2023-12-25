Claro, vou gerar um código complexo em Lua para você. Aqui está:

```lua
-- Código complexo em Lua

-- Função para calcular o fatorial de um número
local function fatorial(n)
    if n <= 1 then
        return 1
    else
        return n * fatorial(n - 1)
    end
end

-- Função para verificar se um número é primo
local function ehPrimo(n)
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
local function fibonacci(n)
    if n <= 0 then
        return {}
    elseif n == 1 then
        return {0}
    elseif n == 2 then
        return {0, 1}
    else
        local seq = {0, 1}
        for i = 3, n do
            seq[i] = seq[i - 1] + seq[i - 2]
        end
        return seq
    end
end

-- Função para ordenar uma tabela de forma decrescente
local function ordenarDecrescente(tabela)
    table.sort(tabela, function(a, b) return a > b end)
end

-- Função para converter uma string para um código ASCII
local function stringToAscii(texto)
    local ascii = {}
    for i = 1, #texto do
        ascii[i] = string.byte(texto, i)
    end
    return ascii
end

-- Função para converter um código ASCII para uma string
local function asciiToString(ascii)
    local texto = ""
    for i = 1, #ascii do
        texto = texto .. string.char(ascii[i])
    end
    return texto
end

-- Função principal do programa
local function main()
    print("Bem-vindo(a) ao programa complexo em Lua!")
    
    local numero = 5
    print("O fatorial de " .. numero .. " é " .. fatorial(numero))
    
    numero = 7
    if ehPrimo(numero) then
        print(numero .. " é um número primo.")
    else
        print(numero .. " não é um número primo.")
    end
    
    numero = 10
    print("A sequência de Fibonacci com " .. numero .. " termos é:")
    local seqFibonacci = fibonacci(numero)
    for i = 1, #seqFibonacci do
        io.write(seqFibonacci[i] .. " ")
    end
    io.write("\n")
    
    local tabela = {9, 4, 6, 2, 1, 8}
    print("A tabela antes de ser ordenada:")
    for i = 1, #tabela do
        io.write(tabela[i] .. " ")
    end
    io.write("\n")
    
    ordenarDecrescente(tabela)
    
    print("A tabela depois de ser ordenada de forma decrescente:")
    for i = 1, #tabela do
        io.write(tabela[i] .. " ")
    end
    io.write("\n")
    
    local texto = "Olá, mundo!"
    print("O texto '" .. texto .. "' convertido para ASCII é:")
    local ascii = stringToAscii(texto)
    for i = 1, #ascii do
        io.write(ascii[i] .. " ")
    end
    io.write("\n")
    
    print("O código ASCII {72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33} convertido para texto é:")
    local textoAscii = asciiToString({72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33})
    print(textoAscii)
end

-- Chamada da função principal
main()
```

Este código em Lua realiza várias tarefas diferentes:

1. Calcula o fatorial de um número.
2. Verifica se um número é primo.
3. Gera uma sequência de Fibonacci com base em um número de termos.
4. Ordena uma tabela de números de forma decrescente.
5. Converte uma string para um código ASCII.
6. Converte um código ASCII para uma string.

O código começa com a definição das funções `fatorial`, `ehPrimo`, `fibonacci`, `ordenarDecrescente`, `stringToAscii` e `asciiToString`. Em seguida, a função `main` é definida como a função principal do programa.

Dentro da função `main`, são realizadas várias operações utilizando as funções previamente definidas. Os resultados são impressos no console.

Espero que este código complexo em Lua atenda às suas expectativas!