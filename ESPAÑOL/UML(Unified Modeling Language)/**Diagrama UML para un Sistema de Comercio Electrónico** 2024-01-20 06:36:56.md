```
[obj] Cliente {
  - Nombre: String
  - Apellido: String
  - CorreoElectronico: String
}

[obj] Producto {
  - Nombre: String
  - Precio: Double
  - Descripcion: String
  - Existencia: Integer
}

[obj] Pedido {
  - Cliente: Cliente
  - Fecha: Date
  - Estado: String {pendiente, en proceso, completado}
  - LineaDePedido: List<LineaDePedido>
}

[obj] LineaDePedido {
  - Cantidad: Integer
  - Precio: Double
  - Producto: Producto
}

[rel] Cliente --(1)--> Pedido { 1:n }
[rel] Pedido --(1)--> LineaDePedido { 1:n }
[rel] LineaDePedido --(1)--> Producto { 1:1 }

[usecase] CrearCliente {
  - Actor: Cliente
  - Precondición: El cliente no existe en el sistema.
  - Flujo principal:
    1. El cliente proporciona su nombre, apellido y correo electrónico.
    2. El sistema crea un nuevo cliente en la base de datos.
    3. El sistema envía un correo electrónico de confirmación al cliente.
  - Postcondición: El cliente existe en el sistema y tiene un correo electrónico de confirmación.
}

[usecase] CrearProducto {
  - Actor: Administrador
  - Precondición: El producto no existe en el sistema.
  - Flujo principal:
    1. El administrador proporciona el nombre, el precio, la descripción y la existencia del producto.
    2. El sistema crea un nuevo producto en la base de datos.
    3. El sistema actualiza el inventario del producto.
  - Postcondición: El producto existe en el sistema y está disponible para la venta.
}

[usecase] RealizarPedido {
  - Actor: Cliente
  - Precondición: El cliente está registrado en el sistema.
  - Flujo principal:
    1. El cliente selecciona los productos que desea comprar y los agrega a su carrito de compra.
    2. El cliente proporciona su información de envío y pago.
    3. El sistema crea un nuevo pedido y lo procesa.
    4. El sistema envía un correo electrónico de confirmación al cliente.
  - Postcondición: El pedido se ha creado y está siendo procesado.
}

[usecase] GestionarPedido {
  - Actor: Administrador
  - Precondición: El pedido existe en el sistema.
  - Flujo principal:
    1. El administrador puede ver los detalles del pedido.
    2. El administrador puede cambiar el estado del pedido.
    3. El administrador puede enviar un correo electrónico de actualización al cliente.
  - Postcondición: El pedido se ha gestionado correctamente.
}
```

Explicación:

* El código UML anterior define un sistema de comercio electrónico simple.
* El sistema consta de tres clases principales: Cliente, Producto y Pedido.
* La clase Cliente representa a los clientes que compran productos en el sistema.
* La clase Producto representa los productos que se venden en el sistema.
* La clase Pedido representa las compras realizadas por los clientes.
* El sistema también define cuatro relaciones entre las clases:
    * Cliente --(1)--> Pedido { 1:n }: Un cliente puede realizar múltiples pedidos.
    * Pedido --(1)--> LineaDePedido { 1:n }: Un pedido puede contener múltiples líneas de pedido.
    * LineaDePedido --(1)--> Producto { 1:1 }: Una línea de pedido corresponde a un solo producto.
* El código UML también define cuatro casos de uso:
    * CrearCliente: Este caso de uso define el proceso para crear un nuevo cliente en el sistema.
    * CrearProducto: Este caso de uso define el proceso para crear un nuevo producto en el sistema.
    * RealizarPedido: Este caso de uso define el proceso para realizar un pedido de productos en el sistema.
    * GestionarPedido: Este caso de uso define el proceso para gestionar pedidos en el sistema.