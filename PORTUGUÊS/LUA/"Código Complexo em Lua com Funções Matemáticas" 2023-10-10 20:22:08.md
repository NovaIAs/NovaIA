Claro! Aqui está um exemplo de código complexo em Lua:

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
    end

    for i = 2, math.sqrt(n) do
        if n % i == 0 then
            return false
        end
    end

    return true
end

-- Função para calcular o número de Fibonacci de um índice
function fibonacci(n)
    if n == 0 then
        return 0
    elseif n == 1 then
        return 1
    else
        return fibonacci(n - 1) + fibonacci(n - 2)
    end
end

-- Função para ordenar uma tabela de números
function bubbleSort(tabela)
    local n = #tabela
    local troca = true

    while troca do
        troca = false

        for i = 1, n - 1 do
            if tabela[i] > tabela[i + 1] then
                tabela[i], tabela[i + 1] = tabela[i + 1], tabela[i]
                troca = true
            end
        end
    end

    return tabela
end

-- Testando as funções
local numero = 5
print("O fatorial de", numero, "é", fatorial(numero))

numero = 7
if isPrimo(numero) then
    print(numero, "é primo")
else
    print(numero, "não é primo")
end

numero = 10
print("O", numero, "º número de Fibonacci é", fibonacci(numero))

local tabela = {4, 2, 8, 5, 1}
print("Tabela original:", table.concat(tabela, ", "))
tabela = bubbleSort(tabela)
print("Tabela ordenada:", table.concat(tabela, ", "))
```

Este código em Lua contém uma série de funções para realizar diferentes operações matemáticas. 

A primeira função, `fatorial`, calcula o fatorial de um número usando recursão. Em seguida, temos a função `isPrimo`, que verifica se um número é primo ou não. Ela utiliza um loop `for` para verificar se o número é divisível por algum outro número além de 1 e ele mesmo.

A terceira função, `fibonacci`, calcula o número de Fibonacci de um dado índice. Ela também utiliza recursão para calcular os números.

Por fim, temos a função `bubbleSort`, que ordena uma tabela de números usando o algoritmo de ordenação bubble sort. Ela percorre a tabela várias vezes, comparando elementos adjacentes e trocando-os de lugar se estiverem fora de ordem.

No final do código, há alguns testes com as funções, onde são chamadas com diferentes valores para verificar os resultados.

Espero que este exemplo complexo de código em Lua atenda às suas expectativas!