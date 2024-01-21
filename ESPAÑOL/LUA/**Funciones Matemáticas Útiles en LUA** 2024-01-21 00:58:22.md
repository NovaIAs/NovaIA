```lua
-- Función para calcular el factorial de un número
function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n - 1)
    end
end

-- Función para calcular la potencia de un número
function potencia(x, n)
    if n == 0 then
        return 1
    else
        return x * potencia(x, n - 1)
    end
end

-- Función para calcular la raíz cuadrada de un número
function raizCuadrada(x)
    if x < 0 then
        error("No se puede calcular la raíz cuadrada de un número negativo")
    end
    local guess = x / 2
    local resultado = 0
    while abs(resultado - guess) > 0.001 do
        resultado = (guess + x / guess) / 2
        guess = resultado
    end
    return resultado
end

-- Función para calcular la distancia entre dos puntos en un espacio bidimensional
function distancia(x1, y1, x2, y2)
    return sqrt((x2 - x1)^2 + (y2 - y1)^2)
end

-- Función para calcular el perímetro de un círculo
function perimetroCirculo(radio)
    return 2 * pi * radio
end

-- Función para calcular el área de un círculo
function areaCirculo(radio)
    return pi * radio^2
end

-- Función para calcular el volumen de una esfera
function volumenEsfera(radio)
    return (4/3) * pi * radio^3
end

-- Función para calcular el área de un triángulo
function areaTriangulo(base, altura)
    return (base * altura) / 2
end

-- Función para calcular el volumen de un prisma rectangular
function volumenPrismaRectangular(largo, ancho, alto)
    return largo * ancho * alto
end

-- Función para calcular el área de un cilindro
function areaCilindro(radio, altura)
    return 2 * pi * radio * (radio + altura)
end

-- Función para calcular el volumen de un cilindro
function volumenCilindro(radio, altura)
    return pi * radio^2 * altura
end

-- Función para calcular el área de un cono
function areaCono(radio, altura)
    return pi * radio * sqrt(radio^2 + altura^2)
end

-- Función para calcular el volumen de un cono
function volumenCono(radio, altura)
    return (1/3) * pi * radio^2 * altura
end
```

Este código contiene una variedad de funciones matemáticas útiles, incluyendo funciones para calcular el factorial de un número, la potencia de un número, la raíz cuadrada de un número, la distancia entre dos puntos, el perímetro de un círculo, el área de un círculo, el volumen de una esfera, el área de un triángulo, el volumen de un prisma rectangular, el área de un cilindro, el volumen de un cilindro, el área de un cono y el volumen de un cono.