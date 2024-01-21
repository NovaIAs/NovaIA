```
+--------------------------------+
|                              |
|      --- Sistema de Gestión --- |
|      +----------------+      |
|                              |
+--------------------------------+

         +---------------------+
         |                     |
         |     Entidades     |
         |                     |
         +---------------------+

               +----------------+
               |                |
               |   Cliente      |
               |                |
               +----------------+

                             +--------------+
                             |              |
                             | Pedido       |
                             |              |
                             +--------------+

                                 +----------------+
                                 |                |
                                 |   Producto     |
                                 |                |
                                 +----------------+

         +---------------------+
         |                     |
         |    Relaciones     |
         |                     |
         +---------------------+

               +--------------+          +----------------+
               |              |          |                |
               |   Cliente   |------>    |   Pedido      |
               |              |          |                |
               +--------------+          +----------------+

                             +--------------+              +----------------+
                             |              |              |                |
                             |   Pedido    |-------->     |   Producto     |
                             |              |              |                |
                             +--------------+              +----------------+

+--------------------------------+
|                              |
|     --- Funcionalidades --- |
|     +----------------+      |
|                              |
+--------------------------------+

               +--------------------+
               |                    |
               |   Alta de Cliente  |
               |                    |
               +--------------------+

               +--------------------+
               |                    |
               |   Baja de Cliente  |
               |                    |
               +--------------------+

               +--------------------+
               |                    |
               |  Modificación de Cliente |
               |                    |
               +--------------------+

               +--------------------+
               |                    |
               |   Alta de Pedido   |
               |                    |
               +--------------------+

               +--------------------+
               |                    |
               |   Baja de Pedido   |
               |                    |
               +--------------------+

               +--------------------+
               |                    |
               |  Modificación de Pedido |
               |                    |
               +--------------------+

               +--------------------+
               |                    |
               |   Alta de Producto  |
               |                    |
               +--------------------+

               +--------------------+
               |                    |
               |   Baja de Producto  |
               |                    |
               +--------------------+

               +--------------------+
               |                    |
               |  Modificación de Producto |
               |                    |
               +--------------------+

+--------------------------------+
|                              |
|      --- Clases ---       |
|      +----------------+      |
|                              |
+--------------------------------+

               +----------------+
               |                |
               |   Cliente     |
               |                |
               +----------------+

                             +--------------+
                             |              |
                             |   Pedido     |
                             |              |
                             +--------------+

                                 +----------------+
                                 |                |
                                 |   Producto    |
                                 |                |
                                 +----------------+

```

Este código UML representa un sistema de gestión de pedidos. El sistema consta de tres entidades: Cliente, Pedido y Producto.

* La entidad Cliente representa a los clientes del sistema. Cada cliente tiene un nombre, una dirección y un número de teléfono.
* La entidad Pedido representa los pedidos realizados por los clientes. Cada pedido tiene un número de pedido, una fecha de pedido y un importe total.
* La entidad Producto representa los productos vendidos por el sistema. Cada producto tiene un nombre, una descripción y un precio.

El sistema también consta de una serie de relaciones entre las distintas entidades.

* La relación Cliente-Pedido representa la relación entre los clientes y los pedidos que han realizado. Cada cliente puede realizar varios pedidos, y cada pedido puede ser realizado por un solo cliente.
* La relación Pedido-Producto representa la relación entre los pedidos y los productos que se han vendido en esos pedidos. Cada pedido puede contener varios productos, y cada producto puede venderse en varios pedidos.

El sistema también tiene una serie de funcionalidades que permiten a los usuarios gestionar los clientes, los pedidos y los productos.

* La funcionalidad Alta de Cliente permite a los usuarios crear nuevos clientes en el sistema.
* La funcionalidad Baja de Cliente permite a los usuarios eliminar clientes del sistema.
* La funcionalidad Modificación de Cliente permite a los usuarios modificar los datos de los clientes.
* La funcionalidad Alta de Pedido permite a los usuarios crear nuevos pedidos en el sistema.
* La funcionalidad Baja de Pedido permite a los usuarios eliminar pedidos del sistema.
* La funcionalidad Modificación de Pedido permite a los usuarios modificar los datos de los pedidos.
* La funcionalidad Alta de Producto permite a los usuarios crear nuevos productos en el sistema.
* La funcionalidad Baja de Producto permite a los usuarios eliminar productos del sistema.
* La funcionalidad Modificación de Producto permite a los usuarios modificar los datos de los productos.