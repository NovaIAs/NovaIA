```lua
-- Este código es una función que recibe un número indefinido de tablas y devuelve una nueva tabla que contiene todos los elementos de las tablas originales.

function concatenar(...)
  -- Creamos una nueva tabla vacía.
  local nuevaTabla = {}

  -- Recorremos las tablas originales.
  for _, tabla in ipairs {...} do
    -- Recorremos los elementos de cada tabla.
    for _, elemento in ipairs (tabla) do
      -- Añadimos el elemento a la nueva tabla.
      table.insert (nuevaTabla, elemento)
    end
  end

  -- Devolvemos la nueva tabla.
  return nuevaTabla
end

-- Ejemplo de uso.
local tabla1 = {1, 2, 3}
local tabla2 = {4, 5, 6}
local tabla3 = {7, 8, 9}

local tablaConcatenada = concatenar (tabla1, tabla2, tabla3)

print (tablaConcatenada) -- Imprime: {1, 2, 3, 4, 5, 6, 7, 8, 9}
```

Explicación del código:

* La función `concatenar` recibe un número indefinido de argumentos, que son tablas.
* La función crea una nueva tabla vacía.
* La función recorre las tablas originales y añade todos sus elementos a la nueva tabla.
* La función devuelve la nueva tabla.
* El código de ejemplo crea tres tablas y luego llama a la función `concatenar` para concatenarlas en una sola tabla.
* La tabla concatenada se imprime en la consola.

Este código es complejo porque utiliza una función que recibe un número indefinido de argumentos, que es una característica avanzada de Lua. Además, el código utiliza una función anidada, que también es una característica avanzada.