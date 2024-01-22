```lua
-- Definición de la función sumaComplejos
function sumaComplejos(a, b)
    local real = a.real + b.real
    local imaginario = a.imaginario + b.imaginario
    return {real = real, imaginario = imaginario}
end

-- Definición de la función restaComplejos
function restaComplejos(a, b)
    local real = a.real - b.real
    local imaginario = a.imaginario - b.imaginario
    return {real = real, imaginario = imaginario}
end

-- Definición de la función multiplicaciónComplejos
function multiplicaciónComplejos(a, b)
    local real = (a.real * b.real) - (a.imaginario * b.imaginario)
    local imaginario = (a.real * b.imaginario) + (a.imaginario * b.real)
    return {real = real, imaginario = imaginario}
end

-- Definición de la función divisiónComplejos
function divisiónComplejos(a, b)
    local denominador = (b.real ^ 2) + (b.imaginario ^ 2)
    local real = ((a.real * b.real) + (a.imaginario * b.imaginario)) / denominador
    local imaginario = ((a.imaginario * b.real) - (a.real * b.imaginario)) / denominador
    return {real = real, imaginario = imaginario}
end

-- Definición de la función conjugadoComplejo
function conjugadoComplejo(a)
    return {real = a.real, imaginario = -a.imaginario}
end

-- Definición de la función móduloComplejo
function móduloComplejo(a)
    return math.sqrt((a.real ^ 2) + (a.imaginario ^ 2))
end

-- Definición de la función argumentoComplejo
function argumentoComplejo(a)
    return math.atan2(a.imaginario, a.real)
end

-- Definición de la función exponencialComplejo
function exponencialComplejo(a)
    local real = math.exp(a.real) * math.cos(a.imaginario)
    local imaginario = math.exp(a.real) * math.sin(a.imaginario)
    return {real = real, imaginario = imaginario}
end

-- Definición de la función logaritmoComplejo
function logaritmoComplejo(a)
    local real = math.log(math.sqrt((a.real ^ 2) + (a.imaginario ^ 2)))
    local imaginario = math.atan2(a.imaginario, a.real)
    return {real = real, imaginario = imaginario}
end

-- Definición de la función senoComplejo
function senoComplejo(a)
    local real = math.sin(a.real) * math.cosh(a.imaginario)
    local imaginario = math.cos(a.real) * math.sinh(a.imaginario)
    return {real = real, imaginario = imaginario}
end

-- Definición de la función cosenoComplejo
function cosenoComplejo(a)
    local real = math.cos(a.real) * math.cosh(a.imaginario)
    local imaginario = -math.sin(a.real) * math.sinh(a.imaginario)
    return {real = real, imaginario = imaginario}
end

-- Definición de la función tangenteComplejo
function tangenteComplejo(a)
    local seno = senoComplejo(a)
    local coseno = cosenoComplejo(a)
    return divisiónComplejos(seno, coseno)
end

-- Definición de la función arcoSenoComplejo
function arcoSenoComplejo(a)
    local real = -math.log(math.sqrt((1 - a.real) ^ 2) + a.imaginario)
    local imaginario = -math.asin(a.real)
    return {real = real, imaginario = imaginario}
end

-- Definición de la función arcoCosenoComplejo
function arcoCosenoComplejo(a)
    local real = -math.log(math.sqrt((1 + a.real) ^ 2) - a.imaginario)
    local imaginario = math.acos(a.real)
    return {real = real, imaginario = imaginario}
end

-- Definición de la función arcoTangenteComplejo
function arcoTangenteComplejo(a)
    local real = -math.log((1 + a.real) / (1 - a.real)) / 2
    local imaginario = math.atan(a.imaginario / (1 + a.real))
    return {real = real, imaginario = imaginario}
end

-- Definición de la función potenciaComplejo
function potenciaComplejo(a, b)
    local real = (a.real ^ b.real) * (math.cos(b.imaginario * math.log(a.real)))
    local imaginario = (a.real ^ b.real) * (math.sin(b.imaginario * math.log(a.real)))
    return {real = real, imaginario = imaginario}
end

-- Definición de la función raízComplejo
function raízComplejo(a, b)
    local real = (a.real ^ (1 / b.real)) * (math.cos(a.imaginario / b.real))
    local imaginario = (a.real ^ (1 / b.real)) * (math.sin(a.imaginario / b.real))
    return {real = real, imaginario = imaginario}
end

-- Definición de la función gráficaComplejo
function gráficaComplejo(a)
    local realArray = {}
    local imaginarioArray = {}
    for i = 1, 360 do
        local ángulo = (i / 180) * math.pi
        realArray[#realArray + 1] = a.real * math.cos(ángulo)
        imaginarioArray[#imaginarioArray + 1] = a.real * math.sin(ángulo)
    end
    return {real = realArray, imaginario = imaginarioArray}
end
```

Este código implementa un conjunto completo de operaciones matemáticas para números complejos en LUA. Incluye funciones para sumar, restar, multiplicar, dividir, conjugar, calcular el módulo y el argumento, y tomar el exponencial, logaritmo, seno, coseno, tangente, arcoseno, arcocoseno y arcotangente de un número complejo. También incluye funciones para calcular potencias y raíces de números complejos, y para generar una gráfica de un número complejo en el plano complejo.

Este código es útil para realizar cálculos matemáticos complejos en LUA, y se puede utilizar en una variedad de aplicaciones, como el desarrollo de juegos, la visualización de datos y el procesamiento de señales.