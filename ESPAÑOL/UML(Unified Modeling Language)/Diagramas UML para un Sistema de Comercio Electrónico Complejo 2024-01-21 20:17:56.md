```
Diagrama de Casos de Uso

Actores:
- Usuario
- Administrador

Casos de Uso:
- Iniciar sesión
- Cerrar sesión
- Crear cuenta
- Editar perfil
- Cambiar contraseña
- Buscar productos
- Añadir productos al carrito
- Realizar compra
- Ver historial de compras
- Gestionar productos
- Gestionar usuarios

Diagrama de Clases

Clases:
- Usuario
- Administrador
- Producto
- Carrito
- Compra
- Historial de compras

Atributos:
- Usuario: nombre de usuario, contraseña, correo electrónico, dirección, teléfono
- Administrador: nombre de usuario, contraseña, correo electrónico, dirección, teléfono
- Producto: nombre, descripción, precio, cantidad
- Carrito: productos, cantidad
- Compra: productos, cantidad, precio total, fecha de compra
- Historial de compras: compras

Métodos:
- Usuario: iniciar sesión, cerrar sesión, crear cuenta, editar perfil, cambiar contraseña
- Administrador: gestionar productos, gestionar usuarios
- Producto: getPrecio(), setPrecio(), getCantidad(), setCantidad()
- Carrito: añadirProducto(), eliminarProducto(), getProductos(), getCantidad()
- Compra: realizarCompra(), getProductos(), getCantidad(), getPrecioTotal(), getFechaCompra()
- Historial de compras: getCompras()

Diagrama de Secuencia

Secuencia: Iniciar sesión

1. El usuario introduce su nombre de usuario y contraseña.
2. El sistema comprueba el nombre de usuario y la contraseña en la base de datos.
3. Si el nombre de usuario y la contraseña son correctos, el sistema inicia sesión para el usuario.
4. El sistema muestra la página de inicio al usuario.

Diagrama de Colaboración

Colaboración: Realizar compra

1. El usuario selecciona los productos que desea comprar y los añade al carrito.
2. El usuario hace clic en el botón "Realizar compra".
3. El sistema calcula el precio total de la compra.
4. El usuario introduce sus datos de pago.
5. El sistema procesa el pago y realiza la compra.
6. El sistema muestra una confirmación de la compra al usuario.

Diagrama de Estado

Estado: Usuario

* Inactivo
* Activo
* Bloqueado

Transiciones:

* Inactivo -> Activo: cuando el usuario inicia sesión.
* Activo -> Inactivo: cuando el usuario cierra sesión.
* Activo -> Bloqueado: cuando el usuario intenta iniciar sesión demasiadas veces con una contraseña incorrecta.

```

Este código UML es un ejemplo complejo de un sistema de comercio electrónico. El sistema tiene dos tipos de usuarios: usuarios y administradores. Los usuarios pueden iniciar sesión, cerrar sesión, crear cuentas, editar perfiles, cambiar contraseñas, buscar productos, añadir productos al carrito, realizar compras y ver su historial de compras. Los administradores pueden gestionar productos y usuarios.

El código UML incluye un diagrama de casos de uso, un diagrama de clases, un diagrama de secuencia, un diagrama de colaboración y un diagrama de estado. Estos diagramas se utilizan para documentar los requisitos del sistema, el diseño del sistema y el comportamiento del sistema.

El código UML es una herramienta valiosa para los desarrolladores de software. Los diagramas UML pueden ayudar a los desarrolladores a comprender los requisitos del sistema, diseñar el sistema y depurar el sistema.