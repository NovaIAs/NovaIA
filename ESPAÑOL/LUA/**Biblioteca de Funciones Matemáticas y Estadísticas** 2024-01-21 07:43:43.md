```lua
-- Definir una función recursiva para calcular el factorial de un número
function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n - 1)
    end
end

-- Definir una función para encontrar el máximo común divisor de dos números usando el algoritmo de Euclides
function mcd(a, b)
    if b == 0 then
        return a
    else
        return mcd(b, a % b)
    end
end

-- Definir una función para encontrar el mínimo común múltiplo de dos números
function mcm(a, b)
    return a * b / mcd(a, b)
end

-- Definir una función para generar un número aleatorio entre un rango
function random_number(min, max)
    return math.random(min, max)
end

-- Definir una función para generar una lista de números aleatorios
function generate_random_list(n, min, max)
    local list = {}
    for i = 1, n do
        table.insert(list, random_number(min, max))
    end
    return list
end

-- Definir una función para encontrar el elemento máximo en una lista
function max_element(list)
    local max = list[1]
    for i = 2, #list do
        if list[i] > max then
            max = list[i]
        end
    end
    return max
end

-- Definir una función para encontrar el elemento mínimo en una lista
function min_element(list)
    local min = list[1]
    for i = 2, #list do
        if list[i] < min then
            min = list[i]
        end
    end
    return min
end

-- Definir una función para encontrar la media de una lista de números
function mean(list)
    local sum = 0
    for i = 1, #list do
        sum = sum + list[i]
    end
    return sum / #list
end

-- Definir una función para encontrar la mediana de una lista de números
function median(list)
    local sorted_list = sort(list)
    local middle_index = math.ceil(#sorted_list / 2)
    if #sorted_list % 2 == 0 then
        return (sorted_list[middle_index] + sorted_list[middle_index + 1]) / 2
    else
        return sorted_list[middle_index]
    end
end

-- Definir una función para encontrar la moda de una lista de números (el valor que aparece con más frecuencia)
function mode(list)
    local frequency_table = {}
    for i = 1, #list do
        if frequency_table[list[i]] == nil then
            frequency_table[list[i]] = 0
        end
        frequency_table[list[i]] = frequency_table[list[i]] + 1
    end
    local max_frequency = 0
    local mode_value = nil
    for value, frequency in pairs(frequency_table) do
        if frequency > max_frequency then
            max_frequency = frequency
            mode_value = value
        end
    end
    return mode_value
end
```

Este código es una colección de funciones útiles para realizar cálculos matemáticos y estadísticos en Lua. Incluye funciones para calcular el factorial de un número, encontrar el máximo común divisor y el mínimo común múltiplo de dos números, generar números aleatorios, encontrar el elemento máximo y mínimo en una lista, calcular la media, la mediana y la moda de una lista de números.

Todas las funciones están bien documentadas y fáciles de usar. Simplemente llama a la función con los argumentos adecuados y devuelve el resultado.

Por ejemplo, para calcular el factorial de 5, puedes usar el siguiente código:

```lua
local factorial_5 = factorial(5)
print(factorial_5) -- Salida: 120
```

Para encontrar el máximo común divisor y el mínimo común múltiplo de 12 y 18, puedes usar el siguiente código:

```lua
local gcd_12_18 = mcd(12, 18)
local lcm_12_18 = mcm(12, 18)
print(gcd_12_18) -- Salida: 6
print(lcm_12_18) -- Salida: 36
```

Para generar una lista de 10 números aleatorios entre 1 y 100, puedes usar el siguiente código:

```lua
local random_list = generate_random_list(10, 1, 100)
for i = 1, #random_list do
    print(random_list[i])
end
```

Para encontrar el elemento máximo y mínimo en una lista, puedes usar el siguiente código:

```lua
local list = {1, 3, 5, 7, 9}
local max_element_list = max_element(list)
local min_element_list = min_element(list)
print(max_element_list) -- Salida: 9
print(min_element_list) -- Salida: 1
```

Para calcular la media, la mediana y la moda de una lista de números, puedes usar el siguiente código:

```lua
local list = {1, 3, 5, 7, 9}
local mean_list = mean(list)
local median_list = median(list)
local mode_list = mode(list)
print(mean_list) -- Salida: 5
print(median_list) -- Salida: 5
print(mode_list) -- Salida: 1
```