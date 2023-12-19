Claro! Aqui está um exemplo de um código complexo em Lua:

```lua
-- Função recursiva para calcular o fatorial de um número
function fatorial(n)
    if n == 0 then
        return 1
    else
        return n * fatorial(n-1)
    end
end

-- Função para verificar se um número é primo
function verificarPrimo(n)
    if n <= 1 then
        return false
    else
        for i = 2, math.sqrt(n) do
            if n % i == 0 then
                return false
            end
        end
        return true
    end
end

-- Função para gerar números primos usando o crivo de Eratóstenes
function gerarPrimos(n)
    local primos = {}
    local crivo = {}

    for i = 2, n do
        crivo[i] = true
    end

    for i = 2, math.sqrt(n) do
        if crivo[i] then
            for j = i^2, n, i do
                crivo[j] = false
            end
        end
    end

    for i = 2, n do
        if crivo[i] then
            table.insert(primos, i)
        end
    end

    return primos
end

-- Função para converter uma string em código Morse
function converterMorse(texto)
    local morse = {
        ["A"] = ".-",
        ["B"] = "-...",
        ["C"] = "-.-.",
        -- Restante do alfabeto
    }
    local resultado = ""

    for i = 1, #texto do
        local c = texto:sub(i, i):upper()
        if morse[c] then
            resultado = resultado .. morse[c] .. " "
        end
    end

    return resultado
end

-- Exemplo de uso das funções
print("O fatorial de 5 é: " .. fatorial(5))

local numero = 17
if verificarPrimo(numero) then
    print(numero .. " é primo.")
else
    print(numero .. " não é primo.")
end

local limite = 100
local primos = gerarPrimos(limite)
print("Os números primos até " .. limite .. " são:")
for i = 1, #primos do
    print(primos[i])
end

local texto = "HELLO WORLD"
local codigoMorse = converterMorse(texto)
print("O código Morse para '" .. texto .. "' é: " .. codigoMorse)
```

Neste código em Lua, implementamos várias funções. 

A primeira função é `fatorial`, que usa recursão para calcular o fatorial de um número. 

A segunda função é `verificarPrimo`, que verifica se um número é primo ou não. Ela utiliza um laço de repetição para testar a divisibilidade do número por todos os inteiros menores que a sua raiz quadrada.

A terceira função é `gerarPrimos`, que utiliza o crivo de Eratóstenes para gerar todos os números primos até um limite especificado.

A última função é `converterMorse`, que recebe uma string e a converte em código Morse. Ela utiliza uma tabela para mapear cada caractere ao seu equivalente em código Morse.

No exemplo de uso das funções, calculamos o fatorial de 5, verificamos se um número é primo, geramos os números primos até um limite especificado e convertemos uma string em código Morse. Os resultados são então exibidos no console.

Espero que este código complexo em Lua atenda às suas expectativas!