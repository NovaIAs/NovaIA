```lua
-- Librerías que utilizaremos
require "sqlite3"
require "logger"
require "json"
require "string"

-- Creamos conexión con la base de datos
local db = sqlite3.open("tienda.db")
-- Creamos el logger para registro de eventos
local logger = Logger.new("tienda.log")

-- Función para crear la tabla productos
function crear_tabla_productos()
  -- La instrucción SQL
  local sql = "CREATE TABLE IF NOT EXISTS productos (
    id INTEGER PRIMARY KEY,
    nombre TEXT NOT NULL,
    precio REAL NOT NULL,
    stock INTEGER NOT NULL,
    UNIQUE INDEX idx_nombre (nombre)
  )"
  -- Ejecutamos la instrucción
  db:exec(sql)
  -- Registramos el evento
  logger:info("Tabla 'productos' creada")
end

-- Función para insertar un producto
function insertar_producto(nombre, precio, stock)
  -- La instrucción SQL
  local sql = "INSERT INTO productos (nombre, precio, stock) VALUES (?, ?, ?)"
  -- Preparamos la instrucción
  local stmt = db:prepare(sql)
  -- Ejecutamos la instrucción con los parámetros
  stmt:bind_params({nombre, precio, stock})
  stmt:step()
  -- Registramos el evento
  logger:info(string.format("Producto '%s' insertado", nombre))
  -- Cerramos la instrucción
  stmt:finalize()
end

-- Función para obtener un producto por nombre
function obtener_producto_por_nombre(nombre)
  -- La instrucción SQL
  local sql = "SELECT * FROM productos WHERE nombre = ?"
  -- Preparamos la instrucción
  local stmt = db:prepare(sql)
  -- Ejecutamos la instrucción con el parámetro
  stmt:bind_params({nombre})
  stmt:step()
  -- Obtenemos el registro
  local producto = stmt:get_row()
  -- Cerramos la instrucción
  stmt:finalize()
  -- Devolvemos el registro
  return producto
end

-- Función para obtener todos los productos
function obtener_todos_los_productos()
  -- La instrucción SQL
  local sql = "SELECT * FROM productos"
  -- Preparamos la instrucción
  local stmt = db:prepare(sql)
  -- Ejecutamos la instrucción
  stmt:step()
  -- Obtenemos todos los registros
  local productos = {}
  while stmt:has_row() do
    productos[#productos + 1] = stmt:get_row()
    stmt:step()
  end
  -- Cerramos la instrucción
  stmt:finalize()
  -- Devolvemos los registros
  return productos
end

-- Función para actualizar un producto
function actualizar_producto(id, nombre, precio, stock)
  -- La instrucción SQL
  local sql = "UPDATE productos SET nombre = ?, precio = ?, stock = ? WHERE id = ?"
  -- Preparamos la instrucción
  local stmt = db:prepare(sql)
  -- Ejecutamos la instrucción con los parámetros
  stmt:bind_params({nombre, precio, stock, id})
  stmt:step()
  -- Registramos el evento
  logger:info(string.format("Producto '%s' actualizado", nombre))
  -- Cerramos la instrucción
  stmt:finalize()
end

-- Función para eliminar un producto
function eliminar_producto(id)
  -- La instrucción SQL
  local sql = "DELETE FROM productos WHERE id = ?"
  -- Preparamos la instrucción
  local stmt = db:prepare(sql)
  -- Ejecutamos la instrucción con el parámetro
  stmt:bind_params({id})
  stmt:step()
  -- Registramos el evento
  logger:info(string.format("Producto con id '%d' eliminado", id))
  -- Cerramos la instrucción
  stmt:finalize()
end

-- Llamamos a la función para crear la tabla
crear_tabla_productos()

-- Llamamos a la función para insertar unos productos
insertar_producto("Manzana", 1.50, 10)
insertar_producto("Pera", 2.00, 15)
insertar_producto("Uva", 3.00, 20)

-- Llamamos a la función para obtener un producto por nombre
local producto = obtener_producto_por_nombre("Uva")
print(string.format("Nombre: %s, Precio: %f, Stock: %d", producto.nombre, producto.precio, producto.stock))

-- Llamamos a la función para obtener todos los productos
local productos = obtener_todos_los_productos()
for _, producto in ipairs(productos) do
  print(string.format("Nombre: %s, Precio: %f, Stock: %d", producto.nombre, producto.precio, producto.stock))
end

-- Llamamos a la función para actualizar un producto
actualizar_producto(1, "Manzana", 1.75, 12)
producto = obtener_producto_por_nombre("Manzana")
print(string.format("Nombre: %s, Precio: %f, Stock: %d", producto.nombre, producto.precio, producto.stock))

-- Llamamos a la función para eliminar un producto
eliminar_producto(2)
productos = obtener_todos_los_productos()
for _, producto in ipairs(productos) do
  print(string.format("Nombre: %s, Precio: %f, Stock: %d", producto.nombre, producto.precio, producto.stock))
end

-- Cerramos la conexión con la base de datos
db:close()
```

Explicación del código:

* Primero, incluimos las librerías necesarias para la aplicación.
* Luego, creamos una conexión con la base de datos y un logger para registrar los eventos.
* Definimos las funciones necesarias para crear la tabla, insertar, obtener, actualizar y eliminar productos.
* Llamamos a las funciones para crear la tabla e insertar unos productos.
* Obtenemos un producto por nombre y lo mostramos.
* Obtenemos todos los productos y los mostramos.
* Actualizamos un producto y lo mostramos.
* Eliminamos un producto y mostramos los productos restantes.
* Cerramos la conexión con la base de datos.