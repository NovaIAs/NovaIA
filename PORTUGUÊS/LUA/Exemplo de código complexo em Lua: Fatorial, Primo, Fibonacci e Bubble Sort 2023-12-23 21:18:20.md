Claro! Aqui está um exemplo de código complexo em Lua:

```lua
-- Função para calcular o fatorial de um número
function fatorial(numero)
    if numero == 0 then
        return 1
    else
        return numero * fatorial(numero - 1)
    end
end

-- Função para verificar se um número é primo
function primo(numero)
    if numero <= 1 then
        return false
    elseif numero <= 3 then
        return true
    elseif numero % 2 == 0 or numero % 3 == 0 then
        return false
    else
        local i = 5
        while i * i <= numero do
            if numero % i == 0 or numero % (i + 2) == 0 then
                return false
            end
            i = i + 6
        end
        return true
    end
end

-- Função para calcular a sequência de Fibonacci
function fibonacci(n)
    local a, b = 0, 1
    for i = 1, n do
        print(a)
        a, b = b, a + b
    end
end

-- Função para ordenar uma tabela usando o algoritmo Bubble Sort
function bubbleSort(tabela)
    local n = #tabela
    repeat
        local trocou = false
        for i = 1, n - 1 do
            if tabela[i] > tabela[i + 1] then
                tabela[i], tabela[i + 1] = tabela[i + 1], tabela[i]
                trocou = true
            end
        end
        n = n - 1
    until not trocou
end

-- Chamada das funções com exemplos
print("Fatorial de 5: ", fatorial(5))

print("Verificar se o número 17 é primo: ", primo(17))

print("Sequência de Fibonacci até o 10º termo:")
fibonacci(10)

local tabela = {9, 15, 4, 2, 10}
print("Tabela não ordenada: ", table.concat(tabela, ", "))
bubbleSort(tabela)
print("Tabela ordenada: ", table.concat(tabela, ", "))
```

Neste código em Lua, eu implementei quatro funções diferentes:

1. `fatorial`: Esta função calcula o fatorial de um número usando recursão.
2. `primo`: Esta função verifica se um número é primo.
3. `fibonacci`: Esta função calcula a sequência de Fibonacci até o n-ésimo termo, onde n é um número passado como parâmetro.
4. `bubbleSort`: Esta função ordena uma tabela de números usando o algoritmo Bubble Sort.

Além disso, o código faz chamadas das funções com exemplos para demonstrar o funcionamento de cada uma delas.