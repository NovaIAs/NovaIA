```lua
-- Este código implementa una pequeña base de datos en memoria utilizando tablas de Lua.

-- Definimos la tabla que contendrá los datos.
local database = {}

-- Agregamos algunos datos a la tabla.
database["nombre"] = "Juan"
database["apellido"] = "Pérez"
database["edad"] = 25

-- Creamos una función para consultar los datos de la base de datos.
function get_data(key)
  return database[key]
end

-- Creamos una función para agregar datos a la base de datos.
function set_data(key, value)
  database[key] = value
end

-- Creamos una función para eliminar datos de la base de datos.
function delete_data(key)
  database[key] = nil
end

-- Creamos una función para imprimir los datos de la base de datos.
function print_data()
  for key, value in pairs(database) do
    print(key, ":", value)
  end
end

-- Ejecutamos la función para imprimir los datos de la base de datos.
print_data()
```

Explicación del código:

* La primera línea del código define una tabla vacía llamada `database`. Esta tabla se utilizará para almacenar los datos de la base de datos.
* Las siguientes tres líneas agregan algunos datos a la tabla `database`. El primer elemento de la tabla es el nombre de la persona, el segundo elemento es el apellido de la persona y el tercer elemento es la edad de la persona.
* La siguiente función, `get_data`, se utiliza para consultar los datos de la base de datos. La función toma un argumento, `key`, que es el nombre del elemento de la tabla que deseamos recuperar. La función devuelve el valor del elemento de la tabla especificado por el argumento `key`.
* La siguiente función, `set_data`, se utiliza para agregar datos a la base de datos. La función toma dos argumentos, `key` y `value`. El argumento `key` es el nombre del elemento de la tabla al que deseamos agregar un valor. El argumento `value` es el valor que deseamos agregar al elemento de la tabla.
* La siguiente función, `delete_data`, se utiliza para eliminar datos de la base de datos. La función toma un argumento, `key`, que es el nombre del elemento de la tabla que deseamos eliminar. La función elimina el elemento de la tabla especificado por el argumento `key`.
* La siguiente función, `print_data`, se utiliza para imprimir los datos de la base de datos. La función itera sobre todos los elementos de la tabla `database` y los imprime en la consola.
* La última línea del código llama a la función `print_data` para imprimir los datos de la base de datos.