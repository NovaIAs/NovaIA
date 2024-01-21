```julia

# Función para calcular el área de un triángulo
function area_triángulo(base, altura)
    return (base * altura) / 2
end

# Función para calcular el perímetro de un cuadrado
function perímetro_cuadrado(lado)
    return 4 * lado
end

# Función para calcular el volumen de una esfera
function volumen_esfera(radio)
    return (4/3) * π * radio^3
end

# Función para calcular la distancia entre dos puntos en un plano
function distancia_entre_puntos(x1, y1, x2, y2)
    return sqrt((x2 - x1)^2 + (y2 - y1)^2)
end

# Función para calcular el factorial de un número
function factorial(n)
    if n == 0
        return 1
    else
        return n * factorial(n - 1)
    end
end

# Función para calcular el máximo común divisor de dos números
function mcd(m, n)
    while n != 0
        aux = m
        m = n
        n = aux % n
    end
    return m
end

#Función para convertir grados Celsius a Fahrenheit
function celsius_a_fahrenheit(grados_celsius)
    return (grados_celsius * 9/5) + 32
end

#Función para convertir Fahrenheit a Celsius
function fahrenheit_a_celsius(grados_fahrenheit)
    return (grados_fahrenheit - 32) * 5/9
end

#Programa principal
print("Área de un triángulo:")
base = readline()
altura = readline()
print("El área del triángulo es:", area_triángulo(base, altura))

print("Perímetro de un cuadrado:")
lado = readline()
print("El perímetro del cuadrado es:", perímetro_cuadrado(lado))

print("Volumen de una esfera:")
radio = readline()
print("El volumen de la esfera es:", volumen_esfera(radio))

print("Distancia entre dos puntos:")
x1 = readline()
y1 = readline()
x2 = readline()
y2 = readline()
print("La distancia entre los puntos es:", distancia_entre_puntos(x1, y1, x2, y2))

print("Factorial de un número:")
n = readline()
print("El factorial de", n, "es:", factorial(n))

print("Máximo común divisor de dos números:")
m = readline()
n = readline()
print("El máximo común divisor de", m, "y", n, "es:", mcd(m, n))

print("Convertir grados Celsius a Fahrenheit:")
grados_celsius = readline()
print("La temperatura en Fahrenheit es:", celsius_a_fahrenheit(grados_celsius))

print("Convertir grados Fahrenheit a Celsius:")
grados_fahrenheit = readline()
print("La temperatura en Celsius es:", fahrenheit_a_celsius(grados_fahrenheit))

```

Este código realiza una variedad de operaciones matemáticas y conversiones de unidades. Incluye funciones para calcular el área de un triángulo, el perímetro de un cuadrado, el volumen de una esfera, la distancia entre dos puntos en un plano, el factorial de un número, el máximo común divisor de dos números y la conversión de grados Celsius a Fahrenheit y viceversa.

El programa principal solicita al usuario que introduzca los valores necesarios para cada operación y luego muestra los resultados.