```lua
-- Definición de variables
local num1, num2, resultado

-- Ingreso de datos por parte del usuario
print("Ingrese el primer número:")
num1 = io.read()

print("Ingrese el segundo número:")
num2 = io.read()

-- Realización de la suma
resultado = num1 + num2

-- Impresión del resultado
print("El resultado de la suma es:", resultado)

-- Condicional para determinar si el resultado es par o impar
if resultado % 2 == 0 then
    print("El resultado es par")
else
    print("El resultado es impar")
end

-- Bucle para imprimir los números del 1 al 10
print("Números del 1 al 10:")
for i = 1, 10 do
    print(i)
end

-- Tabla con los nombres de los días de la semana
dias_semana = {
    "Lunes",
    "Martes",
    "Miércoles",
    "Jueves",
    "Viernes",
    "Sábado",
    "Domingo"
}

-- Impresión de los nombres de los días de la semana
print("Días de la semana:")
for i = 1, #dias_semana do
    print(dias_semana[i])
end

-- Función para calcular el factorial de un número
function factorial(n)
    if n == 0 then
        return 1
    else
        return n * factorial(n-1)
    end
end

-- Cálculo y presentación del factorial de 5
print("Factorial de 5:", factorial(5))
```

**Explicación del código:**

1. **Variables:** Se definen las variables `num1`, `num2` y `resultado` para almacenar los números ingresados por el usuario y el resultado de la suma.

2. **Ingreso de datos:** Se utilizan las funciones integradas `print()` e `io.read()` para que el usuario pueda ingresar los dos números.

3. **Suma:** Se realiza la suma de los dos números ingresados y se almacena el resultado en la variable `resultado`.

4. **Impresión del resultado:** Se utiliza la función `print()` para mostrar el resultado de la suma en la consola.

5. **Condicional:** Se utiliza una instrucción condicional `if` para determinar si el resultado es par o impar. Esto se hace mediante el uso del operador `%` (módulo), que devuelve el resto de la división entre dos números. Si el resto es 0, el número es par; de lo contrario, es impar.

6. **Bucle:** Se utiliza un bucle `for` para imprimir los números del 1 al 10. La función `#` devuelve el número de elementos en una tabla, por lo que el bucle se ejecuta 10 veces.

7. **Tabla:** Se define una tabla `dias_semana` con los nombres de los días de la semana.

8. **Impresión de los días de la semana:** Se utiliza otro bucle `for` para imprimir los nombres de los días de la semana. Esta vez, el bucle se ejecuta 7 veces, que es el número de elementos en la tabla `dias_semana`.

9. **Función para calcular el factorial:** Se define una función `factorial()` que calcula el factorial de un número. El factorial de un número es el producto de todos los números enteros positivos menores o iguales que ese número. Por ejemplo, el factorial de 5 es 5 x 4 x 3 x 2 x 1 = 120.

10. **Cálculo y presentación del factorial de 5:** Se utiliza la función `factorial()` para calcular el factorial de 5 y se muestra el resultado en la consola.