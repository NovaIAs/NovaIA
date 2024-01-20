```
Sistema de Gestión de Inventario y Ventas (SGIV)

Diagrama de Clases

Clase Producto
- IdProducto: int
- Nombre: string
- Descripción: string
- Precio: decimal
- Cantidad: int

Clase Cliente
- IdCliente: int
- Nombre: string
- Dirección: string
- Teléfono: string
- CorreoElectrónico: string

Clase Proveedor
- IdProveedor: int
- Nombre: string
- Dirección: string
- Teléfono: string
- CorreoElectrónico: string

Clase Compra
- IdCompra: int
- FechaCompra: date
- TotalCompra: decimal
- ProveedorId: int (Llave Foránea)

Clase Venta
- IdVenta: int
- FechaVenta: date
- TotalVenta: decimal
- ClienteId: int (Llave Foránea)

Clase Factura
- IdFactura: int
- NúmeroFactura: string
- FechaFactura: date
- TotalFactura: decimal
- VentaId: int (Llave Foránea)

Clase DetalleCompra
- IdDetalleCompra: int
- CompraId: int (Llave Foránea)
- ProductoId: int (Llave Foránea)
- Cantidad: int
- PrecioUnitario: decimal
- Total: decimal

Clase DetalleVenta
- IdDetalleVenta: int
- VentaId: int (Llave Foránea)
- ProductoId: int (Llave Foránea)
- Cantidad: int
- PrecioUnitario: decimal
- Total: decimal

Diagrama de Casos de Uso

Actor: Administrador

Caso de Uso: Gestionar Productos
- Agregar Producto
- Modificar Producto
- Eliminar Producto
- Buscar Producto

Actor: Vendedor

Caso de Uso: Gestionar Ventas
- Crear Venta
- Agregar Producto a la Venta
- Eliminar Producto de la Venta
- Finalizar Venta

Actor: Cliente

Caso de Uso: Realizar Compra
- Seleccionar Producto
- Agregar Producto al Carrito
- Eliminar Producto del Carrito
- Finalizar Compra

Diagrama de Secuencia

Caso de Uso: Realizar Compra

1. El cliente selecciona un producto.
2. El sistema muestra el producto al cliente.
3. El cliente agrega el producto al carrito.
4. El sistema actualiza el carrito del cliente.
5. El cliente elimina un producto del carrito.
6. El sistema actualiza el carrito del cliente.
7. El cliente finaliza la compra.
8. El sistema crea una venta.
9. El sistema crea un detalle de venta para cada producto en el carrito del cliente.
10. El sistema muestra la factura al cliente.

Explicación del Código

El código UML anterior representa un sistema de gestión de inventario y ventas. El sistema consta de seis clases: Producto, Cliente, Proveedor, Compra, Venta y Factura. Cada clase tiene sus propios atributos y métodos.

El código también incluye tres diagramas: un diagrama de clases, un diagrama de casos de uso y un diagrama de secuencia. El diagrama de clases muestra las diferentes clases del sistema y sus relaciones. El diagrama de casos de uso muestra los diferentes casos de uso del sistema y los actores que participan en cada caso de uso. El diagrama de secuencia muestra el flujo de eventos en un caso de uso específico.

El sistema es fácil de usar y permite a los administradores, vendedores y clientes gestionar el inventario y las ventas de forma eficiente.