```lua
-- DEFINIMOS UNA FUNCIÓN QUE RECIBE UN NÚMERO Y DEVUELVE SU FACTORIAL
function factorial(n)
  if n == 0 then
    return 1
  else
    return n * factorial(n-1)
  end
end

-- CREAMOS UNA TABLA CON LOS 10 PRIMEROS NÚMEROS NATURALES
numeros = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}

-- CREAMOS UNA TABLA VACÍA PARA ALMACENAR LOS FACTORIALES DE LOS NÚMEROS NATURALES
factoriales = {}

-- RECORREMOS LA TABLA DE NÚMEROS NATURALES Y CALCULAMOS SU FACTORIAL
for i, numero in pairs(numeros) do
  factoriales[i] = factorial(numero)
end

-- IMPRIMIMOS LOS RESULTADOS
print("Número\tFactorial")
for i, numero in pairs(numeros) do
  print(numero, "\t", factoriales[i])
end
```

Explicación del código:

* Definimos una función llamada `factorial` que recibe un número y devuelve su factorial. La función utiliza recursividad, que es una técnica de programación en la que una función se llama a sí misma para resolver un problema. En este caso, la función `factorial` se llama a sí misma para calcular el factorial de un número, utilizando la fórmula factorial(n) = n * factorial(n-1).
* Creamos una tabla llamada `numeros` que contiene los 10 primeros números naturales.
* Creamos una tabla vacía llamada `factoriales` para almacenar los factoriales de los números naturales.
* Recorremos la tabla `numeros` utilizando un bucle `for` y calculamos el factorial de cada número utilizando la función `factorial`.
* Almacenamos los factoriales de los números naturales en la tabla `factoriales`.
* Imprimimos los resultados en la consola utilizando un bucle `for` y la función `print`.