```
Sistema de Gestión de Clientes y Ventas

Actores:
- Cliente: Persona o entidad que adquiere productos o servicios.
- Vendedor: Persona o entidad que vende productos o servicios.
- Administrador: Persona o entidad que gestiona el sistema.

Casos de uso:
- Registrar cliente: Crea un nuevo registro de cliente en el sistema.
- Buscar cliente: Busca un cliente en el sistema por nombre, ID u otros criterios.
- Actualizar cliente: Actualiza los datos de un cliente existente en el sistema.
- Eliminar cliente: Elimina un cliente del sistema.
- Registrar venta: Crea un nuevo registro de venta en el sistema.
- Buscar venta: Busca una venta en el sistema por fecha, ID u otros criterios.
- Actualizar venta: Actualiza los datos de una venta existente en el sistema.
- Eliminar venta: Elimina una venta del sistema.
- Generar informe de ventas: Genera un informe con las ventas realizadas en un periodo determinado.

Clases:
- Cliente: Representa un cliente en el sistema. Incluye información como nombre, dirección, teléfono, correo electrónico, etc.
- Vendedor: Representa un vendedor en el sistema. Incluye información como nombre, dirección, teléfono, correo electrónico, etc.
- Producto: Representa un producto o servicio vendido en el sistema. Incluye información como nombre, descripción, precio, stock, etc.
- Venta: Representa una venta realizada en el sistema. Incluye información como fecha, hora, cliente, vendedor, producto, cantidad, precio total, etc.
- Informe de ventas: Representa un informe con las ventas realizadas en un periodo determinado. Incluye información como fecha, vendedor, producto, cantidad, precio total, etc.

Relaciones:
- Cliente: Vendedor: Relación muchos a muchos. Un cliente puede tener varios vendedores, y un vendedor puede tener varios clientes.
- Cliente: Producto: Relación muchos a muchos. Un cliente puede comprar varios productos, y un producto puede ser comprado por varios clientes.
- Vendedor: Producto: Relación muchos a muchos. Un vendedor puede vender varios productos, y un producto puede ser vendido por varios vendedores.
- Venta: Cliente: Relación uno a muchos. Una venta solo puede tener un cliente.
- Venta: Vendedor: Relación uno a muchos. Una venta solo puede tener un vendedor.
- Venta: Producto: Relación uno a muchos. Una venta puede tener varios productos.
- Venta: Informe de ventas: Relación uno a muchos. Una venta solo puede estar incluida en un informe de ventas.

Diagrama de clases:

```
+----------------+
| Cliente       |
+----------------+
| ID            |
| Nombre        |
| Dirección     |
| Teléfono      |
| Correo electrónico |
+----------------+

+----------------+
| Vendedor      |
+----------------+
| ID            |
| Nombre        |
| Dirección     |
| Teléfono      |
| Correo electrónico |
+----------------+

+----------------+
| Producto       |
+----------------+
| ID            |
| Nombre        |
| Descripción   |
| Precio        |
| Stock         |
+----------------+

+-----------------+
| Venta           |
+-----------------+
| ID            |
| Fecha          |
| Hora           |
| Cliente        |
| Vendedor       |
| Producto       |
| Cantidad       |
| Precio total   |
+-----------------+

+-----------------------+
| Informe de ventas    |
+-----------------------+
| ID                  |
| Fecha inicial       |
| Fecha final         |
| Vendedor           |
| Producto           |
| Cantidad           |
| Precio total        |
+-----------------------+

+-------------------+
| Cliente: Vendedor |
+-------------------+
| Cliente ID       |
| Vendedor ID     |
+-------------------+

+--------------------+
| Cliente: Producto |
+--------------------+
| Cliente ID       |
| Producto ID     |
+--------------------+

+--------------------+
| Vendedor: Producto |
+--------------------+
| Vendedor ID     |
| Producto ID     |
+--------------------+

+----------------+
| Venta: Cliente |
+----------------+
| Venta ID      |
| Cliente ID    |
+----------------+

+----------------+
| Venta: Vendedor |
+----------------+
| Venta ID      |
| Vendedor ID  |
+----------------+

+----------------+
| Venta: Producto |
+----------------+
| Venta ID      |
| Producto ID   |
+----------------+

+---------------------------+
| Venta: Informe de ventas |
+---------------------------+
| Venta ID                |
| Informe de ventas ID   |
+---------------------------+
```

Explicación del código:

El diagrama de clases muestra las clases y sus relaciones en el sistema. Las clases están representadas por rectángulos, y las relaciones están representadas por líneas. Las relaciones se etiquetan con el tipo de relación, como "uno a muchos" o "muchos a muchos".

El caso de uso "Registrar cliente" se implementa por la clase "Cliente". El caso de uso "Buscar cliente" se implementa por el método "buscarCliente" en la clase "Cliente". El caso de uso "Actualizar cliente" se implementa por el método "actualizarCliente" en la clase "Cliente". El caso de uso "Eliminar cliente" se implementa por el método "eliminarCliente" en la clase "Cliente".

El caso de uso "Registrar venta" se implementa por la clase "Venta". El caso de uso "Buscar venta" se implementa por el método "buscarVenta" en la clase "Venta". El caso de uso "Actualizar venta" se implementa por el método "actualizarVenta" en la clase "Venta". El caso de uso "Eliminar venta"