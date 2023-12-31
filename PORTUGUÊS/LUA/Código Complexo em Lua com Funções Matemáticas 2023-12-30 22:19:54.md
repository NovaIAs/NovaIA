Claro! Aqui está um código complexo escrito em Lua:

```lua
-- Função para calcular o fatorial de um número
function fatorial(numero)
    if numero <= 0 then
        return 1
    else
        return numero * fatorial(numero - 1)
    end
end

-- Função para encontrar o número de Fibonacci em uma determinada posição
function fibonacci(posicao)
    if posicao == 0 then
        return 0
    elseif posicao == 1 then
        return 1
    else
        return fibonacci(posicao - 1) + fibonacci(posicao - 2)
    end
end

-- Função para verificar se um número é primo
function isPrimo(numero)
    if numero <= 1 then
        return false
    end
    for i = 2, numero - 1 do
        if numero % i == 0 then
            return false
        end
    end
    return true
end

-- Função para ordenar uma tabela de números de forma crescente
function ordenarTabela(tabela)
    local tamanho = #tabela
    for i = 1, tamanho - 1 do
        for j = i + 1, tamanho do
            if tabela[i] > tabela[j] then
                tabela[i], tabela[j] = tabela[j], tabela[i]
            end
        end
    end
    return tabela
end

-- Exemplo de uso das funções
local numero = 5
local fatorialNumero = fatorial(numero)
print("O fatorial de " .. numero .. " é " .. fatorialNumero)

local posicao = 6
local fibonacciNumero = fibonacci(posicao)
print("O número de Fibonacci na posição " .. posicao .. " é " .. fibonacciNumero)

local tabela = {9, 4, 7, 2, 1, 8, 3, 6, 5}
local tabelaOrdenada = ordenarTabela(tabela)
print("A tabela ordenada é:")
for i = 1, #tabelaOrdenada do
    print(tabelaOrdenada[i])
end

local numeroPrimo = 17
if isPrimo(numeroPrimo) then
    print(numeroPrimo .. " é um número primo")
else
    print(numeroPrimo .. " não é um número primo")
end
```

Neste código em Lua, são apresentadas algumas funções úteis. 

A função `fatorial` recebe um número como entrada e retorna o fatorial desse número. Ela utiliza uma estrutura de decisão `if-else` e uma chamada recursiva para calcular o fatorial.

A função `fibonacci` recebe uma posição como entrada e retorna o número de Fibonacci correspondente a essa posição. Ela também utiliza uma estrutura de decisão `if-else` e chamadas recursivas para calcular o número de Fibonacci.

A função `isPrimo` verifica se um número é primo. Ela utiliza uma estrutura de decisão `if-else` e um loop `for` para percorrer os possíveis divisores do número.

A função `ordenarTabela` recebe uma tabela de números como entrada e a ordena de forma crescente. Ela utiliza dois loops `for` aninhados e uma estrutura de decisão `if` para fazer as comparações e trocas necessárias para ordenar a tabela.

No exemplo de uso das funções, são exibidos os resultados obtidos para diferentes casos de teste. O código mostra como chamar as funções e exibir os resultados utilizando a função `print`.