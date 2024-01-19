```
Diagrama de Clases:

Clase Cliente:
- Atributos:
  - idCliente: int
  - nombreCliente: string
  - direccionCliente: string
  - telefonoCliente: string
- Métodos:
  - crearCliente()
  - modificarCliente()
  - eliminarCliente()
  - consultarCliente()

Clase Producto:
- Atributos:
  - idProducto: int
  - nombreProducto: string
  - descripcionProducto: string
  - precioProducto: float
- Métodos:
  - crearProducto()
  - modificarProducto()
  - eliminarProducto()
  - consultarProducto()

Clase Pedido:
- Atributos:
  - idPedido: int
  - fechaPedido: date
  - estadoPedido: string
  - totalPedido: float
- Métodos:
  - crearPedido()
  - modificarPedido()
  - eliminarPedido()
  - consultarPedido()

Diagrama de Secuencia:

1. El cliente inicia sesión en el sistema.
2. El cliente selecciona un producto del catálogo.
3. El cliente agrega el producto al carrito de compras.
4. El cliente procede al pago.
5. El cliente ingresa sus datos de pago.
6. El sistema procesa el pago.
7. El sistema genera una factura.
8. El sistema envía la factura al cliente por correo electrónico.

Diagrama de Actividad:

1. Inicio
2. Cliente inicia sesión en el sistema
3. Cliente selecciona un producto del catálogo
4. Cliente agrega el producto al carrito de compras
5. Cliente procede al pago
6. Cliente ingresa sus datos de pago
7. Sistema procesa el pago
8. Sistema genera una factura
9. Sistema envía la factura al cliente por correo electrónico
10. Fin

Diagrama de Casos de Uso:

Actor: Cliente
Caso de Uso: Comprar un producto

Flujo Principal:

1. El cliente inicia sesión en el sistema.
2. El cliente selecciona un producto del catálogo.
3. El cliente agrega el producto al carrito de compras.
4. El cliente procede al pago.
5. El cliente ingresa sus datos de pago.
6. El sistema procesa el pago.
7. El sistema genera una factura.
8. El sistema envía la factura al cliente por correo electrónico.

Flujos Alternativos:

1. El cliente no tiene una cuenta.
2. El cliente no tiene productos en el carrito de compras.
3. El cliente no tiene suficiente dinero para pagar el producto.
4. El sistema no puede procesar el pago.
```

Este código UML representa un sistema de comercio electrónico en el que los clientes pueden iniciar sesión, seleccionar productos del catálogo, agregarlos al carrito de compras, proceder al pago, ingresar sus datos de pago y finalmente el sistema procesa el pago, genera una factura y la envía al cliente por correo electrónico. El diagrama de clases muestra las clases principales del sistema, el diagrama de secuencia muestra la interacción entre los objetos del sistema, el diagrama de actividad muestra el flujo de actividades del sistema y el diagrama de casos de uso muestra el caso de uso principal del sistema.