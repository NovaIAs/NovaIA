```lua
-- Módulo para la manipulation de números complexos

-- Función para crear un número complejo
function complejo(real, imaginario)
    return {real = real, imaginario = imaginario}
end

-- Función para sumar dos números complejos
function suma_complejos(num1, num2)
    return complejo(num1.real + num2.real, num1.imaginario + num2.imaginario)
end

-- Función para restar dos números complejos
function resta_complejos(num1, num2)
    return complejo(num1.real - num2.real, num1.imaginario - num2.imaginario)
end

-- Función para multiplicar dos números complejos
function multiplicacion_complejos(num1, num2)
    return complejo(
        num1.real * num2.real - num1.imaginario * num2.imaginario,
        num1.real * num2.imaginario + num1.imaginario * num2.real
    )
end

-- Función para dividir dos números complejos
function division_complejos(num1, num2)
    -- Calcular el conjugado de num2
    num2_conjugado = complejo(num2.real, -num2.imaginario)
    
    -- Multiplicar num1 por el conjugado de num2
    producto = multiplicacion_complejos(num1, num2_conjugado)
    
    -- Dividir el producto por el módulo cuadrado de num2
    modulo_num2_cuadrado = num2.real^2 + num2.imaginario^2
    return complejo(producto.real / modulo_num2_cuadrado, producto.imaginario / modulo_num2_cuadrado)
end

-- Función para imprimir un número complejo en formato (a + bi)
function imprimir_complejo(num)
    local sign = "+"
    if num.imaginario < 0 then
        sign = "-"
    end
    
    print(string.format("%s + %si", num.real, math.abs(num.imaginario), sign))
end

-- Pedir al usuario que introduzca los valores para dos números complejos
print("Introduce los valores para el primer número complejo (real, imaginario):")
num1_real = tonumber(io.read())
num1_imaginario = tonumber(io.read())

print("Introduce los valores para el segundo número complejo (real, imaginario):")
num2_real = tonumber(io.read())
num2_imaginario = tonumber(io.read())

-- Crear los números complejos
num1 = complejo(num1_real, num1_imaginario)
num2 = complejo(num2_real, num2_imaginario)

-- Sumar los números complejos
suma = suma_complejos(num1, num2)

-- Restar los números complejos
resta = resta_complejos(num1, num2)

-- Multiplicar los números complejos
multiplicacion = multiplicacion_complejos(num1, num2)

-- Dividir los números complejos
division = division_complejos(num1, num2)

-- Imprimir los resultados
print("Suma:")
imprimir_complejo(suma)

print("Resta:")
imprimir_complejo(resta)

print("Multiplicación:")
imprimir_complejo(multiplicacion)

print("División:")
imprimir_complejo(division)
```

Explicación del código:

1. Definimos la función `complejo()` para crear un número complejo a partir de sus partes real e imaginaria.
2. Definimos las funciones `suma_complejos()`, `resta_complejos()`, `multiplicacion_complejos()` y `division_complejos()` para realizar las operaciones aritméticas básicas entre números complejos.
3. Definimos la función `imprimir_complejo()` para imprimir un número complejo en el formato "a + bi", donde "a" es la parte real, "b" es la parte imaginaria y "i" es la unidad imaginaria.
4. Pedimos al usuario que introduzca los valores para dos números complejos utilizando la función `io.read()`.
5. Creamos los objetos `num1` y `num2` utilizando la función `complejo()` para almacenar los valores introducidos por el usuario.
6. Llamamos a las funciones `suma_complejos()`, `resta_complejos()`, `multiplicacion_complejos()` y `division_complejos()` para realizar las operaciones aritméticas entre los números complejos.
7. Llamamos a la función `imprimir_complejo()` para imprimir los resultados de las operaciones.