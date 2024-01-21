```lua
-- Definimos una función que recibe una tabla y un valor y devuelve el índice del valor en la tabla o -1 si no se encuentra.
function buscar(tabla, valor)
  for i, v in ipairs(tabla) do
    if v == valor then
      return i
    end
  end
  return -1
end

-- Creamos una tabla con algunos datos.
datos = {"uno", "dos", "tres", "cuatro", "cinco"}

-- Mostramos los datos de la tabla.
print("Datos:")
for i, v in ipairs(datos) do
  print(i, ":", v)
end

-- Pedimos al usuario que introduzca un valor a buscar.
print("Introduzca un valor a buscar:")
valor = io.read()

-- Buscamos el valor en la tabla.
indice = buscar(datos, valor)

-- Mostramos el resultado de la búsqueda.
if indice == -1 then
  print("El valor no se encuentra en la tabla.")
else
  print("El valor se encuentra en la tabla en el índice", indice)
end
```

Explicación del código:

* La función `buscar` recorre todos los elementos de la tabla utilizando el bucle `for` y devuelve el índice del valor si lo encuentra. Si no lo encuentra, devuelve -1.
* La tabla `datos` contiene una lista de cinco valores.
* El bucle `for` muestra los datos de la tabla, recorriendo cada elemento de la tabla e imprimiendo su índice y su valor.
* La variable `valor` almacena el valor que el usuario introduce.
* La variable `indice` almacena el índice del valor en la tabla o -1 si no se encuentra.
* El condicional `if` muestra el resultado de la búsqueda. Si el índice es -1, el valor no se encuentra en la tabla. Si el índice es mayor o igual que 1, el valor se encuentra en la tabla en el índice indicado.