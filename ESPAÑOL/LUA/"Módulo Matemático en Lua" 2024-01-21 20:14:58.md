```lua
-- Módulo de funciones matemáticas.
local math = {}

-- Función para calcular el máximo común divisor (MCD) de dos números.
math.mcd = function(a, b)
    while b ~= 0 do
        local temp = b
        b = a % b
        a = temp
    end
    return a
end

-- Función para calcular el mínimo común múltiplo (MCM) de dos números.
math.mcm = function(a, b)
    return (a * b) / math.mcd(a, b)
end

-- Función para calcular la raíz cuadrada de un número.
math.sqrt = function(x)
    if x < 0 then
        error("No se puede calcular la raíz cuadrada de un número negativo")
    end
    local low = 0
    local high = x
    local guess = (low + high) / 2
    while math.abs(guess * guess - x) > 1e-6 do
        if guess * guess < x then
            low = guess
        else
            high = guess
        end
        guess = (low + high) / 2
    end
    return guess
end

-- Función para calcular el factorial de un número.
math.factorial = function(n)
    if n < 0 then
        error("No se puede calcular el factorial de un número negativo")
    end
    local result = 1
    for i = 1, n do
        result = result * i
    end
    return result
end

-- Función para calcular el número de combinaciones de n elementos tomados de k en k.
math.combinaciones = function(n, k)
    if n < k or k < 0 then
        error("Los parámetros deben ser números enteros positivos y n debe ser mayor o igual que k")
    end
    return math.factorial(n) / (math.factorial(k) * math.factorial(n - k))
end

-- Función para calcular el número de permutaciones de n elementos tomados de k en k.
math.permutaciones = function(n, k)
    if n < k or k < 0 then
        error("Los parámetros deben ser números enteros positivos y n debe ser mayor o igual que k")
    end
    return math.factorial(n) / math.factorial(n - k)
end

-- Función para calcular el seno de un ángulo en radianes.
math.sen = function(x)
    local result = 0
    local term = x
    local sign = 1
    for i = 1, 100 do
        result = result + term
        term = term * x^2 / (2 * i * (2 * i + 1))
        sign = -sign
    end
    return result
end

-- Función para calcular el coseno de un ángulo en radianes.
math.cos = function(x)
    local result = 1
    local term = 1
    local sign = 1
    for i = 1, 100 do
        result = result + term
        term = term * x^2 / ((2 * i - 1) * 2 * i)
        sign = -sign
    end
    return result
end

-- Función para calcular la tangente de un ángulo en radianes.
math.tan = function(x)
    return math.sen(x) / math.cos(x)
end

-- Función para calcular el arco seno de un número.
math.arcsen = function(x)
    if x < -1 or x > 1 then
        error("El argumento debe estar entre -1 y 1")
    end
    local result = 0
    local term = x
    local sign = 1
    for i = 1, 100 do
        result = result + term
        term = term * x^2 * (2 * i - 1) / (2 * i * (2 * i + 1))
        sign = -sign
    end
    return result
end

-- Función para calcular el arco coseno de un número.
math