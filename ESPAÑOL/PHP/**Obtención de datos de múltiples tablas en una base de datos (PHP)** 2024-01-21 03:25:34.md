```php
<?php

// Definir los nombres de las tablas
$tabla_usuarios = "usuarios";
$tabla_productos = "productos";
$tabla_pedidos = "pedidos";

// Crear la conexión a la base de datos
$conexion = mysqli_connect("localhost", "usuario", "contraseña", "nombre_base_de_datos");

// Comprobar si la conexión se ha realizado correctamente
if (!$conexion) {
  die("Error de conexión a la base de datos: " . mysqli_connect_error());
}

// Crear la consulta para obtener todos los usuarios
$consulta_usuarios = "SELECT * FROM $tabla_usuarios";

// Ejecutar la consulta sobre los usuarios
$resultado_usuarios = mysqli_query($conexion, $consulta_usuarios);

// Comprobar si la consulta se ha realizado correctamente
if (!$resultado_usuarios) {
  die("Error al obtener los usuarios de la base de datos: " . mysqli_error($conexion));
}

// Crear un array para almacenar los usuarios
$usuarios = array();

// Recorrer los resultados de la consulta sobre los usuarios
while ($fila_usuario = mysqli_fetch_assoc($resultado_usuarios)) {
  // Añadir el usuario al array
  $usuarios[] = $fila_usuario;
}

// Crear la consulta para obtener todos los productos
$consulta_productos = "SELECT * FROM $tabla_productos";

// Ejecutar la consulta sobre los productos
$resultado_productos = mysqli_query($conexion, $consulta_productos);

// Comprobar si la consulta se ha realizado correctamente
if (!$resultado_productos) {
  die("Error al obtener los productos de la base de datos: " . mysqli_error($conexion));
}

// Crear un array para almacenar los productos
$productos = array();

// Recorrer los resultados de la consulta sobre los productos
while ($fila_producto = mysqli_fetch_assoc($resultado_productos)) {
  // Añadir el producto al array
  $productos[] = $fila_producto;
}

// Crear la consulta para obtener todos los pedidos
$consulta_pedidos = "SELECT * FROM $tabla_pedidos";

// Ejecutar la consulta sobre los pedidos
$resultado_pedidos = mysqli_query($conexion, $consulta_pedidos);

// Comprobar si la consulta se ha realizado correctamente
if (!$resultado_pedidos) {
  die("Error al obtener los pedidos de la base de datos: " . mysqli_error($conexion));
}

// Crear un array para almacenar los pedidos
$pedidos = array();

// Recorrer los resultados de la consulta sobre los pedidos
while ($fila_pedido = mysqli_fetch_assoc($resultado_pedidos)) {
  // Añadir el pedido al array
  $pedidos[] = $fila_pedido;
}

// Cerrar la conexión a la base de datos
mysqli_close($conexion);

// Mostrar los resultados
echo "Usuarios:";
echo "<pre>";
print_r($usuarios);
echo "</pre>";

echo "Productos:";
echo "<pre>";
print_r($productos);
echo "</pre>";

echo "Pedidos:";
echo "<pre>";
print_r($pedidos);
echo "</pre>";

?>
```

Explicación del código:

* Se definen los nombres de las tablas de la base de datos.
* Se crea la conexión a la base de datos.
* Se crean las consultas para obtener todos los usuarios, productos y pedidos.
* Se ejecutan las consultas.
* Se comprueban los resultados de las consultas.
* Se crean arrays para almacenar los usuarios, productos y pedidos.
* Se recorren los resultados de las consultas y se añaden los usuarios, productos y pedidos a los arrays correspondientes.
* Se cierra la conexión a la base de datos.
* Se muestran los resultados.