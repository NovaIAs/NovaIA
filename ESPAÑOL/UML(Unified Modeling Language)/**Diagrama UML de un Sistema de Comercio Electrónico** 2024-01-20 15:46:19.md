```
Diagrama de Casos de Uso:

Actor: Usuario

Casos de Uso:

1. Iniciar Sesión: El usuario ingresa su nombre de usuario y contraseña para acceder al sistema.
2. Crear Cuenta: El usuario proporciona su información personal y crea una cuenta nueva.
3. Buscar Productos: El usuario busca productos por nombre, categoría o precio.
4. Ver Detalles del Producto: El usuario ve los detalles de un producto, incluyendo su descripción, precio e imágenes.
5. Añadir al Carrito: El usuario añade un producto al carrito de compras.
6. Eliminar del Carrito: El usuario elimina un producto del carrito de compras.
7. Actualizar Cantidad: El usuario actualiza la cantidad de un producto en el carrito de compras.
8. Finalizar Compra: El usuario finaliza la compra e ingresa su información de pago.
9. Ver Historial de Compras: El usuario ve su historial de compras anteriores.
10. Editar Perfil: El usuario edita su información personal.

Diagrama de Clases:

Clase Usuario:

Atributos:
- idUsuario: Identificador único del usuario.
- nombre: Nombre del usuario.
- correoElectronico: Correo electrónico del usuario.
- contrasena: Contraseña del usuario.

Métodos:
- iniciarSesion(): Inicia sesión en el sistema.
- crearCuenta(): Crea una cuenta nueva.
- buscarProductos(): Busca productos por nombre, categoría o precio.
- verDetallesProducto(): Ve los detalles de un producto.
- anadirAlCarrito(): Añade un producto al carrito de compras.
- eliminarDelCarrito(): Elimina un producto del carrito de compras.
- actualizarCantidad(): Actualiza la cantidad de un producto en el carrito de compras.
- finalizarCompra(): Finaliza la compra e ingresa su información de pago.
- verHistorialCompras(): Ve su historial de compras anteriores.
- editarPerfil(): Edita su información personal.

Clase Producto:

Atributos:
- idProducto: Identificador único del producto.
- nombre: Nombre del producto.
- descripcion: Descripción del producto.
- precio: Precio del producto.
- categoria: Categoría del producto.
- imagenes: Imágenes del producto.

Métodos:
- buscarProductos(): Busca productos por nombre, categoría o precio.
- verDetallesProducto(): Ve los detalles de un producto.
- anadirAlCarrito(): Añade un producto al carrito de compras.
- eliminarDelCarrito(): Elimina un producto del carrito de compras.
- actualizarCantidad(): Actualiza la cantidad de un producto en el carrito de compras.

Clase Carrito:

Atributos:
- idCarrito: Identificador único del carrito.
- usuario: Usuario al que pertenece el carrito.
- productos: Productos que contiene el carrito.
- cantidad: Cantidad de cada producto en el carrito.
- total: Total a pagar por los productos del carrito.

Métodos:
- anadirAlCarrito(): Añade un producto al carrito de compras.
- eliminarDelCarrito(): Elimina un producto del carrito de compras.
- actualizarCantidad(): Actualiza la cantidad de un producto en el carrito de compras.
- finalizarCompra(): Finaliza la compra e ingresa su información de pago.

Clase Compra:

Atributos:
- idCompra: Identificador único de la compra.
- usuario: Usuario que realizó la compra.
- productos: Productos que se compraron.
- cantidad: Cantidad de cada producto que se compró.
- total: Total pagado por los productos.
- fecha: Fecha en que se realizó la compra.

Métodos:
- verHistorialCompras(): Ve el historial de compras anteriores.

Diagrama de Secuencia:

1. El usuario ingresa su nombre de usuario y contraseña e inicia sesión en el sistema.
2. El usuario busca productos por nombre, categoría o precio.
3. El usuario ve los detalles de un producto y lo añade al carrito de compras.
4. El usuario finaliza la compra e ingresa su información de pago.
5. El sistema procesa la compra y almacena los detalles en la base de datos.
6. El usuario recibe una confirmación de la compra por correo electrónico.

Diagrama de Colaboración:

El usuario, el producto, el carrito y la compra colaboran para realizar una compra.

El usuario busca productos y los añade al carrito.

El carrito almacena los productos y calcula el total a pagar.

El usuario finaliza la compra e ingresa su información de pago.

El sistema procesa la compra y almacena los detalles en la base de datos.

El usuario recibe una confirmación de la compra por correo electrónico.
```

Este código representa un sistema de comercio electrónico simple en UML. El sistema permite a los usuarios iniciar sesión, crear cuentas, buscar productos, ver detalles de los productos, añadir productos al carrito de compras, eliminar productos del carrito de compras, actualizar la cantidad de productos en el carrito de compras, finalizar la compra e ingresar su información de pago. El sistema también permite a los usuarios ver su historial de compras y editar su información personal.

El código está organizado en cuatro diagramas: diagrama de casos de uso, diagrama de clases, diagrama de secuencia y diagrama de colaboración.

El diagrama de casos de uso describe los casos de uso del sistema, es decir, las tareas que los usuarios pueden realizar con el sistema.

El diagrama de clases describe las clases del sistema, es decir, los objetos que el sistema utiliza para representar datos.

El diagrama de secuencia describe la secuencia de mensajes que se intercambian entre los objetos del sistema para realizar un caso de uso.

El diagrama de colaboración describe la colaboración entre los objetos del sistema para realizar un caso de uso.