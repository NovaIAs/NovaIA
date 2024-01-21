```lua
-- Definimos una función llamada "saludar" que recibe un nombre como parámetro.
function saludar(nombre)
  -- Imprimimos un mensaje de saludo utilizando el nombre que recibimos como parámetro.
  print("¡Hola, " .. nombre .. "!")
end

-- Definimos una tabla llamada "nombres" que contiene una lista de nombres.
nombres = {"Juan", "María", "Pedro", "Ana", "Luis"}

-- Recorremos la tabla "nombres" utilizando un bucle "for".
for i, nombre in ipairs(nombres) do
  -- Llamamos a la función "saludar" para saludar a cada nombre de la tabla.
  saludar(nombre)
end

-- Definimos una función llamada "suma" que recibe dos números como parámetros.
function suma(n1, n2)
  -- Devolvemos la suma de los dos números que recibimos como parámetros.
  return n1 + n2
end

-- Definimos una variable llamada "resultado" que contiene el resultado de llamar a la función "suma" con los números 5 y 10.
resultado = suma(5, 10)

-- Imprimimos el resultado de la suma utilizando la variable "resultado".
print("La suma de 5 y 10 es: " .. resultado)

-- Definimos una tabla llamada "frutas" que contiene una lista de frutas.
frutas = {"Manzana", "Naranja", "Plátano", "Uva", "Pera"}

-- Recorremos la tabla "frutas" utilizando un bucle "while".
while #frutas > 0 do
  -- Eliminamos el primer elemento de la tabla "frutas".
  frutas = table.remove(frutas, 1)

  -- Imprimimos el primer elemento de la tabla "frutas".
  print("La primera fruta de la lista es: " .. frutas[1])
end

-- Definimos una función llamada "factorial" que recibe un número como parámetro.
function factorial(n)
  -- Si el número es igual a 0, devolvemos 1.
  if n == 0 then
    return 1
  end

  -- De lo contrario, devolvemos el número multiplicado por el factorial del número menos uno.
  return n * factorial(n - 1)
end

-- Definimos una variable llamada "factorialResultado" que contiene el resultado de llamar a la función "factorial" con el número 5.
factorialResultado = factorial(5)

-- Imprimimos el resultado del factorial utilizando la variable "factorialResultado".
print("El factorial de 5 es: " .. factorialResultado)

```

Este código es un ejemplo complejo de código LUA que demuestra una variedad de características del lenguaje, incluyendo:

* Definición de funciones.
* Uso de bucles "for" y "while".
* Eliminación de elementos de una tabla.
* Uso de funciones recursivas.

El código también incluye algunos comentarios en español para ayudar a explicar lo que está haciendo.