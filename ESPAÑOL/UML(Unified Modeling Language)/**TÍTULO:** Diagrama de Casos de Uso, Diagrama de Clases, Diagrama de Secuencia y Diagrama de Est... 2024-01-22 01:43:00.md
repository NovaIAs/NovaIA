**Diagrama de Casos de Uso**

* Actores:
    * Usuario
    * Administrador
* Casos de uso:
    * Registrarse
    * Iniciar sesión
    * Cerrar sesión
    * Buscar productos
    * Ver detalles del producto
    * Agregar producto al carrito
    * Quitar producto del carrito
    * Realizar compra
    * Ver historial de compras
    * Administrar productos
    * Administrar usuarios

**Diagrama de Clases**

* Clases:
    * Usuario
    * Administrador
    * Producto
    * Carrito
    * Compra
    * Historial de compras
* Relaciones:
    * Usuario tiene muchos Carritos
    * Carrito tiene muchos Productos
    * Compra tiene muchos Productos
    * Usuario tiene muchos Historiales de compras
    * Administrador administra Productos
    * Administrador administra Usuarios

**Diagrama de Secuencia**

* Caso de uso: Realizar compra
* Pasos:
    1. Usuario selecciona productos y los agrega al carrito.
    2. Usuario inicia sesión o se registra.
    3. Usuario ingresa su información de pago.
    4. Sistema procesa el pago y crea una compra.
    5. Sistema envía un correo electrónico al usuario con la confirmación de la compra.

**Diagrama de Estados**

* Clase: Producto
* Estados:
    * Disponible
    * Agotado
    * Descontinuado
* Transiciones:
    * Disponible -> Agotado: cuando el producto se agota
    * Agotado -> Disponible: cuando el producto se repone
    * Disponible -> Descontinuado: cuando el producto se descontinúa