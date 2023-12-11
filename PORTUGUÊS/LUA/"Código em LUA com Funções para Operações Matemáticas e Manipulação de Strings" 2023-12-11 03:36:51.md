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
function isPrimo(n)
    if n <= 1 then
        return false
    elseif n <= 3 then
        return true
    elseif n % 2 == 0 or n % 3 == 0 then
        return false
    end

    local i = 5
    while i * i <= n do
        if n % i == 0 or n % (i + 2) == 0 then
            return false
        end
        i = i + 6
    end

    return true
end

-- Função para imprimir os números primos menores ou iguais a um valor limite
function imprimirPrimos(limite)
    for i = 2, limite do
        if isPrimo(i) then
            print(i)
        end
    end
end

-- Função para verificar se um número é par
function isPar(n)
    return n % 2 == 0
end

-- Função para calcular o quadrado de um número
function quadrado(n)
    return n * n
end

-- Função para verificar se um número é perfeito
function isPerfeito(n)
    local soma = 0
    for i = 1, n-1 do
        if n % i == 0 then
            soma = soma + i
        end
    end
    return soma == n
end

-- Função para somar dois números
function somar(a, b)
    return a + b
end

-- Função para subtrair dois números
function subtrair(a, b)
    return a - b
end

-- Função para multiplicar dois números
function multiplicar(a, b)
    return a * b
end

-- Função para dividir dois números
function dividir(a, b)
    if b == 0 then
        return "Erro: divisão por zero"
    else
        return a / b
    end
end

-- Função para calcular a média de uma lista de números
function media(lista)
    local soma = 0
    for i = 1, #lista do
        soma = soma + lista[i]
    end
    return soma / #lista
end

-- Função para calcular o máximo de uma lista de números
function maximo(lista)
    local max = lista[1]
    for i = 2, #lista do
        if lista[i] > max then
            max = lista[i]
        end
    end
    return max
end

-- Função para calcular o mínimo de uma lista de números
function minimo(lista)
    local min = lista[1]
    for i = 2, #lista do
        if lista[i] < min then
            min = lista[i]
        end
    end
    return min
end

-- Função para verificar se uma palavra é um palíndromo
function isPalindromo(palavra)
    local palavraInvertida = ""
    for i = #palavra, 1, -1 do
        palavraInvertida = palavraInvertida .. palavra:sub(i, i)
    end
    return palavra == palavraInvertida
end

-- Função para embaralhar uma string
function embaralharString(palavra)
    local palavraEmbaralhada = ""
    local indices = {}
    for i = 1, #palavra do
        table.insert(indices, i)
    end
    while #indices > 0 do
        local indiceAleatorio = math.random(1, #indices)
        palavraEmbaralhada = palavraEmbaralhada .. palavra:sub(indices[indiceAleatorio], indices[indiceAleatorio])
        table.remove(indices, indiceAleatorio)
    end
    return palavraEmbaralhada
end

-- Exemplo de uso das funções
local numero = 5
print(string.format("O fatorial de %d é %d", numero, fatorial(numero)))

local limitePrimos = 20
print("Os números primos menores ou iguais a " .. limitePrimos .. " são:")
imprimirPrimos(limitePrimos)

local numeroPar = 6
if isPar(numeroPar) then
    print(numeroPar .. " é par")
else
    print(numeroPar .. " é ímpar")
end

local numeroQuadrado = 4
print(string.format("O quadrado de %d é %d", numeroQuadrado, quadrado(numeroQuadrado)))

local numeroPerfeito = 6
if isPerfeito(numeroPerfeito) then
    print(numeroPerfeito .. " é um número perfeito")
else
    print(numeroPerfeito .. " não é um número perfeito")
end

local a = 5
local b = 3
print(string.format("A soma de %d e %d é %d", a, b, somar(a, b)))
print(string.format("A subtração de %d por %d é %d", a, b, subtrair(a, b)))
print(string.format("A multiplicação de %d por %d é %d", a, b, multiplicar(a, b)))
print(string.format("A divisão de %d por %d é %s", a, b, dividir(a, b)))

local listaNumeros = {1, 2, 3, 4, 5}
print(string.format("A média dos números %s é %f", table.concat(listaNumeros, ", "), media(listaNumeros)))
print(string.format("O máximo dos números %s é %d", table.concat(listaNumeros, ", "), maximo(listaNumeros)))
print(string.format("O mínimo dos números %s é %d", table.concat(listaNumeros, ", "), minimo(listaNumeros)))

local palavraPalindromo = "arara"
if isPalindromo(palavraPalindromo) then
    print(palavraPalindromo .. " é um palíndromo")
else
    print(palavraPalindromo .. " não é um palíndromo")
end

local palavraEmbaralhada = "lua"
print(string.format("A palavra %s embaralhada é %s", palavraEmbaralhada, embaralharString(palavraEmbaralhada))))
```

Este código em LUA contém uma série de funções que executam diferentes operações matemáticas e manipulações de strings. Ele inclui funções para calcular o fatorial de um número, verificar se um número é primo, par, perfeito ou palíndromo, além de funções para realizar operações matemáticas simples como soma, subtração, multiplicação e divisão. Também há funções para calcular a média, o máximo e o mínimo de uma lista de números, bem como para embaralhar uma string. O código inclui exemplos de uso das funções para demonstrar seu funcionamento.