```lua
-- Creamos una función recursiva para calcular el factorial de un número.
function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n-1)
    end
end

-- Creamos una función para generar una secuencia de Fibonacci.
function fibonacci(n)
    if n <= 1 then
        return n
    else
        return fibonacci(n-1) + fibonacci(n-2)
    end
end

-- Creamos una función para buscar el elemento más grande en una lista.
function max(lista)
    local maximo = lista[1]
    for i = 2, #lista do
        if lista[i] > maximo then
            maximo = lista[i]
        end
    end
    return maximo
end

-- Creamos una función para buscar el elemento más pequeño en una lista.
function min(lista)
    local minimo = lista[1]
    for i = 2, #lista do
        if lista[i] < minimo then
            minimo = lista[i]
        end
    end
    return minimo
end

-- Creamos una función para ordenar una lista en orden ascendente.
function sortAsc(lista)
    for i = 1, #lista-1 do
        for j = i+1, #lista do
            if lista[i] > lista[j] then
                local aux = lista[i]
                lista[i] = lista[j]
                lista[j] = aux
            end
        end
    end
    return lista
end

-- Creamos una función para ordenar una lista en orden descendente.
function sortDesc(lista)
    for i = 1, #lista-1 do
        for j = i+1, #lista do
            if lista[i] < lista[j] then
                local aux = lista[i]
                lista[i] = lista[j]
                lista[j] = aux
            end
        end
    end
    return lista
end

-- Creamos una función para buscar un elemento en una lista.
function find(lista, elemento)
    for i = 1, #lista do
        if lista[i] == elemento then
            return i
        end
    end
    return -1
end

-- Creamos una función para eliminar un elemento de una lista.
function remove(lista, elemento)
    local index = find(lista, elemento)
    if index ~= -1 then
        for i = index, #lista-1 do
            lista[i] = lista[i+1]
        end
        lista[#lista] = nil
        return true
    else
        return false
    end
end

-- Creamos una función para insertar un elemento en una lista.
function insert(lista, elemento, index)
    if index == nil or index < 1 or index > #lista+1 then
        index = #lista+1
    end
    local newLista = {}
    for i = 1, index-1 do
        newLista[i] = lista[i]
    end
    newLista[index] = elemento
    for i = index+1, #lista do
        newLista[i+1] = lista[i]
    end
    return newLista
end

-- Creamos una función para invertir una lista.
function reverse(lista)
    local newLista = {}
    for i = #lista, 1, -1 do
        newLista[#newLista+1] = lista[i]
    end
    return newLista
end

-- Creamos una función para concatenar dos listas.
function concat(lista1, lista2)
    local newLista = {}
    for i = 1, #lista1 do
        newLista[#newLista+1] = lista1[i]
    end
    for i = 1, #lista2 do
        newLista[#newLista+1] = lista2[i]
    end
    return newLista
end

-- Creamos una función para dividir una lista en dos sublistas.
function split(lista, index)
    local subLista1 = {}
    local subLista2 = {}
    for i = 1, index do
        subLista1[#subLista1+1] = lista[i]
    end
    for i = index+1, #lista do
        subLista2[#subLista2+1] = lista[i]
    end
    return subLista1, subLista2
end

-- Creamos una función para generar una lista de números aleatorios.
function randomList(n, min, max)
    local lista = {}
    for i = 1, n do
        lista[#lista+1] = math.random(min, max)
    end
    return lista
end

-- Creamos una función para generar una lista de números pares.
function evenList(n)
    local lista = {}
    for i = 1, n do
        lista[#lista+1] = 2*i
    end
    return lista
end

-- Creamos una función para generar una lista de números impares.
function oddList(n)
    local lista = {}
    for i = 1, n do
        lista[#lista+1] = 2*i-1
    end
    return lista
end

-- Creamos una función para generar una lista de números primos.
function primeList(n)
    local lista = {}
    local count = 0
    local i = 2
    while count < n do
        local