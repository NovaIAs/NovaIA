## Diagrama de Clases

```
+----------------+
| Clase Persona |
+----------------+
| - id: int    |
| - nombre: str |
| - apellido: str|
| - edad: int    |
| -----------------------------------|
| + crear(nombre, apellido, edad)    |
| + modificar(nombre, apellido, edad) |
| + eliminar()                        |
| + obtener_id()                      |
| + obtener_nombre()                   |
| + obtener_apellido()                  |
| + obtener_edad()                     |
+------------------------------------

+--------------------+
| Clase Empleado    |
+--------------------+
| - id: int           |
| - nombre: str        |
| - apellido: str     |
| - salario: float     |
| - departamento: str  |
| -------------------------|
| + crear(nombre, apellido, salario, departamento) |
| + modificar(nombre, apellido, salario, departamento) |
| + eliminar()                                     |
| + obtener_id()                                   |
| + obtener_nombre()                                |
| + obtener_apellido()                               |
| + obtener_salario()                                |
| + obtener_departamento()                            |
+----------------------------------------------

+---------------------+
| Clase Cliente      |
+---------------------+
| - id: int           |
| - nombre: str        |
| - apellido: str     |
| - direccion: str    |
| - telefono: int      |
| ------------------------|
| + crear(nombre, apellido, direccion, telefono)   |
| + modificar(nombre, apellido, direccion, telefono) |
| + eliminar()                                   |
| + obtener_id()                                 |
| + obtener_nombre()                              |
| + obtener_apellido()                            |
| + obtener_direccion()                          |
| + obtener_telefono()                            |
+----------------------------------------------

+---------------------+
| Clase Producto     |
+---------------------+
| - id: int           |
| - nombre: str        |
| - descripcion: str  |
| - precio: float      |
| - stock: int        |
| -------------------------|
| + crear(nombre, descripcion, precio, stock)         |
| + modificar(nombre, descripcion, precio, stock)      |
| + eliminar()                                      |
| + obtener_id()                                    |
| + obtener_nombre()                                 |
| + obtener_descripcion()                            |
| + obtener_precio()                                 |
| + obtener_stock()                                  |
+------------------------------------------------

+-------------------+
| Clase Inventario  |
+-------------------+
| - id: int           |
| - producto: Producto |
| - cantidad: int      |
| -----------------------|
| + crear(producto, cantidad)     |
| + modificar(producto, cantidad)  |
| + eliminar()                   |
| + obtener_id()                 |
| + obtener_producto()            |
| + obtener_cantidad()            |
+---------------------------------

+----------------+
| Clase Pedido  |
+----------------+
| - id: int              |
| - cliente: Cliente      |
| - productos: List[Producto] |
| - total: float          |
| - estado: str            |
| -----------------------------|
| + crear(cliente, productos) |
| + modificar(cliente, productos) |
| + eliminar()                |
| + obtener_id()              |
| + obtener_cliente()         |
| + obtener_productos()       |
| + obtener_total()           |
| + obtener_estado()          |
+-----------------------------

+----------------------+
| Clase Factura      |
+----------------------+
| - id: int             |
| - pedido: Pedido      |
| - fecha: str           |
| - total: float        |
| - pagado: bool         |
| ---------------------------|
| + crear(pedido, fecha)    |
| + modificar(pedido, fecha) |
| + eliminar()              |
| + obtener_id()            |
| + obtener_pedido()        |
| + obtener_fecha()         |
| + obtener_total()         |
| + obtener_pagado()        |
+---------------------------
```

## Explicación del Código

El código de UML anterior representa un modelo de un sistema de gestión de pedidos. El sistema consta de las siguientes clases:

* **Persona:** Representa a una persona, ya sea un empleado o un cliente.
* **Empleado:** Representa a un empleado de la empresa.
* **Cliente:** Representa a un cliente de la empresa.
* **Producto:** Representa un producto que se vende en la empresa.
* **Inventario:** Representa el inventario de productos de la empresa.
* **Pedido:** Representa un pedido realizado por un cliente.
* **Factura:** Representa una factura emitida por la empresa a un cliente.

Las clases están relacionadas entre sí a través de asociaciones. Las asociaciones pueden ser de uno a uno, de uno a muchos o de muchos a muchos. Las asociaciones de uno a uno se representan con una línea sólida, las asociaciones de uno a muchos con una línea discontinua y las asociaciones de muchos a muchos con una línea discontinua doble.

Por ejemplo, la clase **Persona** tiene una asociación de uno a uno con la clase **Empleado** y con la clase **Cliente**. Esto significa que una persona sólo puede ser un empleado o un cliente, pero no ambos. La clase **Pedido** tiene una asociación de uno a muchos con la clase **Producto**. Esto significa que un pedido puede tener varios productos, pero un producto sólo puede estar en un pedido. La clase **Factura** tiene una asociación de uno a uno con la clase **Pedido**. Esto significa que una factura sólo puede emitirse para un pedido, y un pedido sólo puede tener una factura.

El código de UML anterior es sólo un ejemplo de cómo se puede modelar un sistema utilizando UML. UML es un lenguaje muy poderoso y se puede utilizar para modelar sistemas de cualquier tamaño y complejidad.