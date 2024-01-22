**Diagrama de Clases**

```
+------------------------------------------+
| **Clase** Cliente                         |
|------------------------------------------|
| **Atributos**                              |
| - Nombre                                 |
| - Dirección                               |
| - Teléfono                               |
| - Correo electrónico                   |
|------------------------------------------|
| **Métodos**                               |
| - RealizarCompra()                        |
| - ConsultarSaldo()                       |
| - CambiarDirección()                     |
| - CambiarTeléfono()                      |
| - CambiarCorreoElectrónico()             |
|------------------------------------------|

+------------------------------------------+
| **Clase** Producto                        |
|------------------------------------------|
| **Atributos**                              |
| - Nombre                                 |
| - Descripción                            |
| - Precio                                |
| - Cantidad                               |
|------------------------------------------|
| **Métodos**                               |
| - Vender()                                |
| - ConsultarPrecio()                      |
| - ConsultarCantidad()                     |
| - CambiarPrecio()                         |
| - CambiarCantidad()                      |
|------------------------------------------|

+------------------------------------------+
| **Clase** Compra                         |
|------------------------------------------|
| **Atributos**                              |
| - Fecha                                  |
| - hora                                   |
| - Cliente                                |
| - Producto                               |
| - Cantidad                               |
| - PrecioTotal                             |
|------------------------------------------|
| **Métodos**                               |
| - RealizarCompra()                        |
| - ConsultarCompra()                       |
| - CancelarCompra()                        |
|------------------------------------------|

+------------------------------------------+
| **Clase** Factura                         |
|------------------------------------------|
| **Atributos**                              |
| - Fecha                                  |
| - hora                                   |
| - Cliente                                |
| - Producto                               |
| - Cantidad                               |
| - PrecioTotal                             |
| - Impuestos                               |
| - Total                                  |
|------------------------------------------|
| **Métodos**                               |
| - GenerarFactura()                        |
| - ConsultarFactura()                      |
| - ImprimirFactura()                       |
|------------------------------------------|
```

**Diagrama de Secuencia**

```
1. Cliente realiza una compra.
2. El sistema valida la información del cliente.
3. El sistema valida la información del producto.
4. El sistema verifica la disponibilidad del producto.
5. El sistema reserva el producto.
6. El sistema genera una factura.
7. El cliente paga la factura.
8. El sistema libera el producto.
9. El cliente recibe el producto.
```

**Diagrama de Estados**

```
+------------------------------------------+
| **Estado** Cliente                         |
|------------------------------------------|
| **Estados posibles**                       |
| - Nuevo                                  |
| - Registrado                             |
| - Activo                                |
| - Inactivo                               |
| - Eliminado                              |
|------------------------------------------|
| **Transiciones entre estados**            |
| - Nuevo -> Registrado: cuando el cliente se |
|   registra en el sistema.                 |
| - Registrado -> Activo: cuando el cliente |
|   realiza su primera compra.               |
| - Activo -> Inactivo: cuando el cliente no |
|   realiza ninguna compra durante un período |
|   determinado de tiempo.                 |
| - Inactivo -> Eliminado: cuando el cliente |
|   no realiza ninguna compra durante un período |
|   determinado de tiempo.                 |
|------------------------------------------|
```

**Explicación del código**

El código anterior es un ejemplo de un diagrama de clases, un diagrama de secuencia y un diagrama de estados para un sistema de venta de productos.

El diagrama de clases muestra las clases principales del sistema, sus atributos y métodos. Las clases principales son Cliente, Producto, Compra y Factura.

El diagrama de secuencia muestra cómo los objetos de las diferentes clases interactúan entre sí para realizar una venta.

El diagrama de estados muestra los diferentes estados por los que puede pasar un cliente en el sistema.