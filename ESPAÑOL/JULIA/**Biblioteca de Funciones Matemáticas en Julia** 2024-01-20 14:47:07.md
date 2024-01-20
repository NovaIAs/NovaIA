```julia
# Definir una función para calcular el máximo común divisor (MCD) de dos números
function mcd(a, b)
    while b != 0
        t = a % b
        a = b
        b = t
    end
    return a
end

# Definir una función para calcular el mínimo común múltiplo (MCM) de dos números
function mcm(a, b)
    return (a * b) / mcd(a, b)
end

# Definir una función para generar una lista de números primos hasta un cierto límite
function primos(n)
    lista = [2:n]
    for i in 3:n
        if lista[i] != 0
            for j in i^2:n:n
                lista[j] = 0
            end
        end
    end
    return lista
end

# Definir una función para calcular la factorización prima de un número
function factores_primos(n)
    factores = []
    i = 2
    while n > 1
        if n % i == 0
            factores.append(i)
            n /= i
        else
            i += 1
        end
    end
    return factores
end

# Definir una función para calcular la fórmula general de Bhaskara
function bhaskara(a, b, c)
    discriminante = b^2 - 4*a*c
    if discriminante < 0
        return "No hay soluciones reales"
    elseif discriminante == 0
        return -b / (2*a)
    else
        x1 = (-b + sqrt(discriminante)) / (2*a)
        x2 = (-b - sqrt(discriminante)) / (2*a)
        return [x1, x2]
    end
end

# Definir una función para calcular el área de un círculo
function area_circulo(r)
    return pi * r^2
end

# Definir una función para calcular el perímetro de un círculo
function perimetro_circulo(r)
    return 2 * pi * r
end

# Definir una función para calcular el volumen de una esfera
function volumen_esfera(r)
    return (4/3) * pi * r^3
end

# Definir una función para calcular la superficie de una esfera
function superficie_esfera(r)
    return 4 * pi * r^2
end

# Definir una función para calcular la pendiente de una línea recta
function pendiente(x1, y1, x2, y2)
    return (y2 - y1) / (x2 - x1)
end

# Definir una función para calcular la ordenada en el origen de una línea recta
function ordenada_origen(x1, y1, pendiente)
    return y1 - pendiente * x1
end

# Definir una función para calcular la ecuación de una línea recta
function ecuacion_recta(x1, y1, x2, y2)
    pendiente = (y2 - y1) / (x2 - x1)
    ordenada_origen = y1 - pendiente * x1
    return "y = " * string(pendiente) * "x + " * string(ordenada_origen)
end

# Definir una función para calcular la distancia entre dos puntos
function distancia(x1, y1, x2, y2)
    return sqrt((x2 - x1)^2 + (y2 - y1)^2)
end

# Definir una función para calcular el área de un triángulo
function area_triangulo(b, h)
    return (1/2) * b * h
end

# Definir una función para calcular el área de un rectángulo
function area_rectangulo(b, h)
    return b * h
end

# Definir una función para calcular el área de un cuadrado
function area_cuadrado(l)
    return l^2
end
```