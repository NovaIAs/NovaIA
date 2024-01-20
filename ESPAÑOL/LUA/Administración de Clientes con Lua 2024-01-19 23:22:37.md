```lua
-- Creación de una tabla para almacenar datos de clientes
clientes = {}

-- Función para agregar un nuevo cliente a la tabla
function nuevo_cliente(nombre, apellidos, correo_electronico)
  -- Crear una nueva entrada en la tabla para el cliente
  clientes[nombre .. " " .. apellidos] = {
    nombre = nombre,
    apellidos = apellidos,
    correo_electronico = correo_electronico
  }
end

-- Función para buscar un cliente por su nombre completo
function buscar_cliente(nombre_completo)
  -- Comprobar si el cliente existe en la tabla
  if clientes[nombre_completo] then
    -- Devolver los datos del cliente
    return clientes[nombre_completo]
  else
    -- Devolver nil si el cliente no existe
    return nil
  end
end

-- Función para eliminar un cliente de la tabla
function eliminar_cliente(nombre_completo)
  
  -- Comprobar si el cliente existe en la tabla
  if clientes[nombre_completo] then
    -- Eliminar el cliente de la tabla
    clientes[nombre_completo] = nil
  end
end

-- Función para imprimir todos los clientes en la tabla
function imprimir_clientes()
  -- Iterar sobre la tabla de clientes
  for nombre_completo, datos_cliente in pairs(clientes) do
    -- Imprimir los datos del cliente
    print(nombre_completo)
    print("Nombre:", datos_cliente.nombre)
    print("Apellidos:", datos_cliente.apellidos)
    print("Correo electrónico:", datos_cliente.correo_electronico)
    print()
  end
end

-- Ejemplo de uso

-- Agregar algunos clientes a la tabla
nuevo_cliente("Juan", "Pérez", "juan.perez@ejemplo.com")
nuevo_cliente("María", "García", "maria.garcia@ejemplo.com")
nuevo_cliente("Pedro", "Fernández", "pedro.fernandez@ejemplo.com")

-- Buscar un cliente por su nombre completo
cliente = buscar_cliente("Juan Pérez")
if cliente then
  print("Cliente encontrado:")
  print("Nombre:", cliente.nombre)
  print("Apellidos:", cliente.apellidos)
  print("Correo electrónico:", cliente.correo_electronico)
else
  print("Cliente no encontrado.")
end

-- Eliminar un cliente de la tabla
eliminar_cliente("Pedro Fernández")

-- Imprimir todos los clientes en la tabla
imprimir_clientes()
```

Explicación del código:

* La función `nuevo_cliente()` crea una nueva entrada en la tabla `clientes` para almacenar los datos de un nuevo cliente.
* La función `buscar_cliente()` busca un cliente en la tabla `clientes` por su nombre completo y devuelve los datos del cliente si existe, o `nil` si no existe.
* La función `eliminar_cliente()` elimina un cliente de la tabla `clientes` por su nombre completo.
* La función `imprimir_clientes()` itera sobre la tabla `clientes` e imprime los datos de cada cliente.

El código de ejemplo crea tres clientes, busca uno de ellos, elimina otro y luego imprime todos los clientes en la tabla.