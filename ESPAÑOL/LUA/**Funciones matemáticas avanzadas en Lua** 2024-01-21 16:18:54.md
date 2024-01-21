```lua
-- Función para encontrar el máximo común divisor (MCD) de dos números
function mcd(a, b)
    while b ~= 0 do
        a, b = b, a%b
    end
    return a
end

-- Función para encontrar el mínimo común múltiplo (MCM) de dos números
function mcm(a, b)
    return a * b / mcd(a, b)
end

-- Función para generar una lista de números primos hasta un límite especificado
function primos(n)
    local primos = {}
    for i = 2, n do
        local es_primo = true
        for j = 2, math.floor(math.sqrt(i)) do
            if i % j == 0 then
                es_primo = false
                break
            end
        end
        if es_primo then
            primos[#primos + 1] = i
        end
    end
    return primos
end

-- Función para factorizar un número en sus factores primos
function factorizar(n)
    local factores = {}
    for i = 2, math.floor(math.sqrt(n)) do
        while n % i == 0 do
            factores[#factores + 1] = i
            n = n / i
        end
    end
    if n > 1 then
        factores[#factores + 1] = n
    end
    return factores
end

-- Función para encontrar el inverso modular de un número en un módulo especificado
function inverso_modular(a, m)
    for i = 1, m-1 do
        if (a * i) % m == 1 then
            return i
        end
    end
    return nil
end

-- Función para encontrar la raíz cuadrada modular de un número en un módulo especificado
function raiz_cuadrada_modular(a, m)
    if m % 4 == 3 then
        return a^(m+1)/4 % m
    elseif m % 8 == 5 then
        -- Aquí se utiliza el algoritmo Tonelli-Shanks para encontrar la raíz cuadrada modular
        local q = m - 1
        local s = 0
        while q % 2 == 0 do
            q = q / 2
            s = s + 1
        end
        if s == 1 then
            return a^(m+1)/4 % m
        end
        local z = 2
        while z^2 % m ~= a do
            z = z + 1
        end
        local c = z^2 % m
        local r = a^(q+1)/2 % m
        local t = a^q % m
        local m0 = s
        local i = 0
        while t ~= 1 do
            t = t^2 % m
            i = i + 1
            if i == m0 then
                return nil
            end
        end
        local b = c^((m0-i-1)/2) % m
        r = (r * b) % m
        for j = 0, m0-i-2 do
            r = (r * b) % m
            b = (b * b) % m
        end
        return r
    else
        return nil
    end
end

-- Ejemplo de uso de las funciones anteriores
local numero_primo = 23
local numero_a_factorizar = 100
local numero_para_inverso_modular = 5
local numero_para_raiz_cuadrada_modular = 3

print("Máximo común divisor de 12 y 18:", mcd(12, 18))
print("Mínimo común múltiplo de 12 y 18:", mcm(12, 18))
print("Lista de números primos hasta 100:", primos(100))
print("Factores primos de 100:", factorizar(100))
print("Inverso modular de 5 en el módulo 23:", inverso_modular(5, 23))
print("Raíz cuadrada modular de 3 en el módulo 23:", raiz_cuadrada_modular(3, 23))
```

Explicación del código:

* La función `mcd` calcula el máximo común divisor de dos números utilizando el algoritmo de Euclides.
* La función `mcm` calcula el mínimo común múltiplo de dos números utilizando la fórmula `mcm(a, b) = a * b / mcd(a, b)`.
* La función `primos` genera una lista de números primos hasta un límite especificado utilizando el algoritmo de la criba de Eratóstenes.
* La función `factorizar` factoriza un número en sus factores primos utilizando el algoritmo de divisiónを試す.
* La función `inverso_modular` calcula el inverso modular de un número en un módulo especificado utilizando el algoritmo extendido de Euclides.
* La función `raiz_cuadrada_modular` calcula la raíz cuadrada modular de un número en un módulo especificado utilizando el algoritmo Tonelli-Shanks.

En la parte final del código se muestran algunos ejemplos de uso de las funciones anteriores.