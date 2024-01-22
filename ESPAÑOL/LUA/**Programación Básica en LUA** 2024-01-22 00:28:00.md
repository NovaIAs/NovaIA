```lua
-- Creación de librerías
local math = require "math"
local string = require "string"
local table = require "table"

-- Variables
local mensaje = "Hola mundo"
local numero = 5
local array = {1, 2, 3}
local diccionario = {nombre = "Juan", edad = 20}

-- Funciones
function suma(a, b)
  return a + b
end

function imprimir(texto)
  io.write(texto)
end

-- Ciclo
for i = 1, 5 do
  imprimir(i)
end

-- Tabla
table.insert(array, 4)
imprimir(table.concat(array, ", "))

-- Condicional
if numero > 3 then
  imprimir("El número es mayor a 3")
elseif numero == 3 then
  imprimir("El número es igual a 3")
else
  imprimir("El número es menor a 3")
end

-- Bucle
while numero > 0 do
  numero = numero - 1
  imprimir(numero)
end

-- Expresión regular
local patron = "^H.*d$"
local resultado = string.match(mensaje, patron)
imprimir(resultado)

-- Gestión de errores
try
  error("Error generado intencionadamente")
catch e
  imprimir(e)
end

-- Finalizar programa
os.exit()
```

Explicación del código:

* **Librerías:** Se incluyen las librerías `math`, `string`, `table` y `os` para utilizar sus funciones.
* **Variables:** Se definen varias variables con diferentes valores.
* **Funciones:** Se definen dos funciones, `suma` e `imprimir`.
* **Ciclo:** Se utiliza un ciclo `for` para imprimir los números del 1 al 5.
* **Tabla:** Se añade un nuevo elemento a la tabla `array` y se imprime el resultado.
* **Condicional:** Se utiliza una sentencia condicional `if-elseif-else` para comprobar el valor de la variable `numero`.
* **Bucle:** Se utiliza un bucle `while` para decrementar el valor de la variable `numero` hasta que sea igual a 0.
* **Expresión regular:** Se utiliza una expresión regular para encontrar el patrón "^H.*d$" en la variable `mensaje`.
* **Gestión de errores:** Se utiliza una sentencia `try-catch` para capturar los errores que se puedan producir en el programa.
* **Finalizar programa:** Se utiliza la función `os.exit()` para finalizar el programa.