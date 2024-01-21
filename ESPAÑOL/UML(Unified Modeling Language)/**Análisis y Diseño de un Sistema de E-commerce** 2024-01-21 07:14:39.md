```

**Diagrama de Clases**

```
Clase Usuario {
    - nombre: String
    - correo: String
    - contraseña: String
    - tipo: String // [administrador, usuario]
}

Clase Producto {
    - id: Int
    - nombre: String
    - descripcion: String
    - precio: Decimal
    - cantidad: Int
    - imagen: String
}

Clase Pedido {
    - id: Int
    - fecha: Date
    - estado: String // [pendiente, aprobado, cancelado, entregado]
    - total: Decimal
    - usuario_id: Int
}

Clase PedidoDetalle {
    - id: Int
    - pedido_id: Int
    - producto_id: Int
    - cantidad: Int
    - precio: Decimal
}

```


**Diagrama de Casos de Uso**

```
Actor: Usuario

Caso de uso: Comprar producto

Descripción: El usuario navega por el catálogo de productos, selecciona un producto y lo añade al carrito. A continuación, introduce sus datos personales y la dirección de envío. Finalmente, confirma el pedido y realiza el pago.

Flujo de eventos:

1. El usuario navega por el catálogo de productos.
2. El usuario selecciona un producto y lo añade al carrito.
3. El usuario hace clic en el botón "Comprar ahora".
4. El usuario introduce sus datos personales y la dirección de envío.
5. El usuario confirma el pedido y realiza el pago.
6. El sistema envía al usuario un correo electrónico de confirmación del pedido.
7. El sistema prepara el pedido y lo envía al usuario.
8. El usuario recibe el pedido y lo revisa.
```


**Diagrama de Secuencia**

```
Usuario -> Sistema: Navegar por el catálogo de productos
Sistema -> Usuario: Mostrar catálogo de productos
Usuario -> Sistema: Seleccionar un producto
Sistema -> Usuario: Mostrar detalles del producto
Usuario -> Sistema: Añadir producto al carrito
Sistema -> Usuario: Mostrar carrito de la compra
Usuario -> Sistema: Introducir datos personales y dirección de envío
Sistema -> Usuario: Mostrar formulario de datos personales y dirección de envío
Usuario -> Sistema: Confirmar pedido y realizar pago
Sistema -> Usuario: Enviar correo electrónico de confirmación del pedido
Sistema -> Transportista: Preparar pedido y enviarlo al usuario
Transportista -> Usuario: Entregar pedido
Usuario -> Transportista: Revisar pedido
```


**Diagrama de Estados**

```
Pedido

* Pendiente
* Aprobado
* Cancelado
* Entregado
```

**Diagrama de Actividad**

```
Actividad: Comprar producto

Inicio

-> Navegar por el catálogo de productos

-> Seleccionar un producto

-> Añadir producto al carrito

-> Hacer clic en el botón "Comprar ahora"

-> Introducir datos personales y dirección de envío

-> Confirmar pedido y realizar el pago

-> El sistema envía al usuario un correo electrónico de confirmación del pedido

-> El sistema prepara el pedido y lo envía al usuario

-> El usuario recibe el pedido y lo revisa

Fin
```