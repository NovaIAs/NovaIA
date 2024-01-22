```
Diagrama de Casos de Uso:

* **Usuario:** 
    * Registrarse
    * Iniciar sesión
    * Buscar productos
    * Agregar productos al carrito
    * Comprar productos
    * Ver pedidos
    * Actualizar información de perfil
* **Administrador:** 
    * Agregar productos
    * Eliminar productos
    * Modificar productos
    * Ver pedidos
    * Generar informes

Diagrama de Clases:

* **Usuario:** 
    * Id
    * Nombre
    * Correo electrónico
    * Contraseña
    * Dirección
    * Teléfono
    * Fecha de nacimiento
* **Producto:** 
    * Id
    * Nombre
    * Descripción
    * Precio
    * Categoría
    * Imagen
* **Carrito:** 
    * Id
    * Usuario
    * Productos
* **Pedido:** 
    * Id
    * Usuario
    * Productos
    * Dirección
    * Teléfono
    * Fecha de entrega
    * Estado del pedido

Diagrama de Secuencia:

* **Caso de uso:** Comprar un producto
    * El usuario inicia sesión.
    * El usuario busca un producto.
    * El usuario agrega el producto al carrito.
    * El usuario va a la página de pago.
    * El usuario ingresa su información de pago.
    * El sistema procesa el pago.
    * El sistema envía una confirmación de pedido al usuario.

Diagrama de Estado:

* **Máquina de estados:** Estado del pedido
    * Estados:
        * Nuevo
        * Procesando
        * Enviado
        * Entregado
        * Cancelado
    * Transiciones:
        * Nuevo -> Procesando
        * Procesando -> Enviado
        * Enviado -> Entregado
        * Enviado -> Cancelado
        * Cancelado -> Nuevo

Diagrama de Actividad:

* **Diagrama de actividad:** Procesar un pedido
    * Actividades:
        * Recibir el pedido
        * Verificar el pago
        * Preparar el producto para el envío
        * Enviar el producto
        * Actualizar el estado del pedido

Diagrama de Comunicación:

* **Diagrama de comunicación:** Sistema de pago
    * Objetos:
        * Usuario
        * Sistema de pago
        * Banco
    * Mensajes:
        * Usuario -> Sistema de pago: Petición de pago
        * Sistema de pago -> Banco: Petición de autorización
        * Banco -> Sistema de pago: Autorización
        * Sistema de pago -> Usuario: Confirmación de pago

```