```lua
-- Función para encontrar el máximo común divisor de dos números
function mcd(a, b)
    if (b == 0) then
        return a
    else
        return mcd(b, a % b)
    end
end

-- Función para encontrar el mínimo común múltiplo de dos números
function mcm(a, b)
    return (a * b) / mcd(a, b)
end

-- Función para encontrar todos los factores de un número
function factores(n)
    local factores = {}
    for i = 1, n do
        if (n % i == 0) then
            factores[i] = true
        end
    end
    return factores
end

-- Función para encontrar todos los divisores de un número
function divisores(n)
    local divisores = {}
    for i = 1, n do
        if (n % i == 0) then
            divisores[i] = true
        end
    end
    return divisores
end

-- Función para encontrar la suma de los dígitos de un número
function suma_digitos(n)
    if (n == 0) then
        return 0
    else
        return (n % 10) + suma_digitos(n / 10)
    end
end

-- Función para encontrar el número de dígitos de un número
function num_digitos(n)
    if (n == 0) then
        return 0
    else
        return 1 + num_digitos(n / 10)
    end
end

-- Función para invertir un número
function invertir_numero(n)
    local invertido = 0
    while (n > 0) do
        invertido = invertido * 10 + (n % 10)
        n = n / 10
    end
    return invertido
end

-- Función para encontrar si un número es primo
function es_primo(n)
    if (n == 1) then
        return false
    end
    for i = 2, n-1 do
        if (n % i == 0) then
            return false
        end
    end
    return true
end

-- Función para encontrar todos los números primos menores o iguales a un número dado
function numeros_primos(n)
    local primos = {}
    for i = 2, n do
        if (es_primo(i)) then
            primos[i] = true
        end
    end
    return primos
end

-- Función para encontrar el siguiente número primo después de un número dado
function siguiente_primo(n)
    while (true) do
        n = n + 1
        if (es_primo(n)) then
            return n
        end
    end
end

-- Función para encontrar el anterior número primo antes de un número dado
function anterior_primo(n)
    while (true) do
        n = n - 1
        if (es_primo(n)) then
            return n
        end
    end
end

-- Función para encontrar el mayor número primo menor que un número dado
function mayor_primo_menor(n)
    while (true) do
        n = n - 1
        if (es_primo(n)) then
            return n
        end
    end
end

-- Función para encontrar el menor número primo mayor que un número dado
function menor_primo_mayor(n)
    while (true) do
        n = n + 1
        if (es_primo(n)) then
            return n