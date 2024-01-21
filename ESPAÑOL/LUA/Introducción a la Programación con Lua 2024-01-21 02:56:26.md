```lua
-- Variables globales
local mensaje = "Hola, mundo!"
local numeros = {1, 2, 3, 4, 5}
local personas = {
  {nombre = "Juan", edad = 25},
  {nombre = "María", edad = 30},
  {nombre = "Pedro", edad = 35}
}

-- Definición de funciones
function saludar()
  print(mensaje)
end

function sumar(a, b)
  return a + b
end

function calcularPromedio(numeros)
  local suma = 0
  for i = 1, #numeros do
    suma = suma + numeros[i]
  end
  return suma / #numeros
end

function obtenerPersonaMayor(personas)
  local personaMayor = nil
  local edadMayor = 0
  for i = 1, #personas do
    if personas[i].edad > edadMayor then
      personaMayor = personas[i]
      edadMayor = personas[i].edad
    end
  end
  return personaMayor
end

-- Llamado a funciones
saludar()
print("La suma de 2 y 3 es:", sumar(2, 3))
print("El promedio de los números 1, 2, 3, 4 y 5 es:", calcularPromedio(numeros))
print("La persona mayor es:", obtenerPersonaMayor(personas))
```

Explicación del código:

1. Variables globales: Se definen las siguientes variables globales:
   - `mensaje`: Un string que contiene el texto "Hola, mundo!".
   - `numeros`: Una tabla que contiene los números 1, 2, 3, 4 y 5.
   - `personas`: Una tabla que contiene tres tablas, cada una de las cuales representa a una persona con un nombre y una edad.

2. Definición de funciones: Se definen las siguientes funciones:
   - `saludar()`: Esta función simplemente imprime el mensaje "Hola, mundo!" en la consola.
   - `sumar(a, b)`: Esta función suma los dos números dados como parámetros y devuelve el resultado.
   - `calcularPromedio(numeros)`: Esta función calcula el promedio de los números contenidos en la tabla dada como parámetro y devuelve el resultado.
   - `obtenerPersonaMayor(personas)`: Esta función encuentra la persona con mayor edad en la tabla dada como parámetro y devuelve esa persona.

3. Llamado a funciones: Se llaman a las funciones definidas anteriormente para mostrar el mensaje "Hola, mundo!", sumar los números 2 y 3, calcular el promedio de los números 1, 2, 3, 4 y 5, y encontrar la persona mayor en la tabla de personas. Los resultados se imprimen en la consola.