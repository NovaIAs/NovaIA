Claro! Aqui está um código complexo em LUA, explicado em detalhes:

```lua
-- Definindo uma função recursiva para calcular o fatorial de um número
function fatorial(n)
    if n == 0 then
        return 1
    else
        return n * fatorial(n - 1)
    end
end

-- Definindo uma função para verificar se um número é primo
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

-- Definindo uma função para ordenar uma lista de números de forma crescente
function bubbleSort(lista)
    local n = #lista
    for i = 1, n do
        for j = 1, n - i do
            if lista[j] > lista[j + 1] then
                lista[j], lista[j + 1] = lista[j + 1], lista[j]
            end
        end
    end
end

-- Exemplo de uso das funções definidas anteriormente
local numero = 5
print("O fatorial de " .. numero .. " é " .. fatorial(numero))
local lista = {5, 2, 7, 1, 9}
print("Lista original: " .. table.concat(lista, ", "))
bubbleSort(lista)
print("Lista ordenada: " .. table.concat(lista, ", "))

-- Exemplo de uso de uma estrutura condicional
local numero = 10
if isPrimo(numero) then
    print(numero .. " é primo")
else
    print(numero .. " não é primo")
end
```

Este código em LUA contém três funções: `fatorial`, `isPrimo` e `bubbleSort`. 

A função `fatorial` é uma função recursiva que calcula o fatorial de um número dado como argumento. Ela utiliza uma estrutura condicional para verificar se o número é igual a zero, caso seja, retorna 1. Caso contrário, retorna o produto do número pelo fatorial do número decrementado em 1.

A função `isPrimo` verifica se um número é primo ou não. Ela utiliza uma estrutura condicional para verificar se o número é menor ou igual a 1. Se for, retorna `false`. Caso contrário, itera de 2 até a raiz quadrada do número e verifica se o número é divisível por algum dos valores no intervalo. Se for, retorna `false`. Se chegar ao final do loop, retorna `true`.

A função `bubbleSort` recebe uma lista de números e a ordena de forma crescente utilizando o algoritmo de ordenação Bubble Sort. Ela utiliza dois loops para percorrer a lista e comparar os valores adjacentes. Se o valor atual for maior que o próximo valor, realiza a troca. O algoritmo repete esse processo até que a lista esteja completamente ordenada.

Após a definição das funções, o código faz uso delas para demonstrar seu funcionamento. Calcula o fatorial de um número específico, ordena uma lista de números e verifica se um número é primo ou não. Os resultados são exibidos no console através da função `print`.

Espero que este código lhe seja útil!