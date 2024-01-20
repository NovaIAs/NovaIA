**Diagrama de Clases:**

```
[Clase] Cliente {
  - Identificador: int
  - Nombre: string
  - Apellidos: string
  - Dirección: string
  - Correo electrónico: string
  - Teléfono: string
}

[Clase] Producto {
  - Identificador: int
  - Nombre: string
  - Descripción: string
  - Precio: double
  - Categoría: string
}

[Clase] Pedido {
  - Identificador: int
  - Cliente: Cliente
  - Fecha: date
  - Estado: string (pendiente, procesando, enviado, entregado)
  - Total: double
}

[Clase] DetallePedido {
  - Identificador: int
  - Pedido: Pedido
  - Producto: Producto
  - Cantidad: int
  - Precio unitario: double
  - Total: double
}
```

**Diagrama de Secuencia:**

```
Cliente -> Sistema: Realizar pedido

Sistema -> Cliente: Confirmar pedido

Cliente -> Sistema: Pagar pedido

Sistema -> Almacén: Preparar pedido

Almacén -> Transportista: Enviar pedido

Transportista -> Cliente: Entregar pedido

Cliente -> Sistema: Confirmar recepción de pedido

Sistema -> Almacén: Actualizar inventario

Sistema -> Cliente: Enviar factura
```

**Diagrama de Casos de Uso:**

```
[Actor] Cliente

[Caso de uso] Realizar pedido

[Flujo principal]

1. Cliente selecciona productos en el sistema.
2. Cliente añade productos al carrito de compras.
3. Cliente introduce sus datos personales y de envío.
4. Cliente elige un método de pago.
5. Cliente realiza el pago.
6. Sistema confirma el pedido al cliente.
7. Sistema envía el pedido al almacén.
8. Almacén prepara el pedido y lo envía al transportista.
9. Transportista entrega el pedido al cliente.
10. Cliente confirma la recepción del pedido.
11. Sistema actualiza el inventario.
12. Sistema envía la factura al cliente.

[Flujo alternativo]

1. Cliente no tiene una cuenta en el sistema.
2. Cliente crea una cuenta en el sistema.
3. Cliente introduce sus datos personales y de envío.
4. Cliente elige un método de pago.
5. Cliente realiza el pago.
6. Sistema confirma el pedido al cliente.
7. Sistema envía el pedido al almacén.
8. Almacén prepara el pedido y lo envía al transportista.
9. Transportista entrega el pedido al cliente.
10. Cliente confirma la recepción del pedido.
11. Sistema actualiza el inventario.
12. Sistema envía la factura al cliente.
```

**Explicación:**

El diagrama de clases muestra las clases principales del sistema, sus atributos y sus relaciones. El diagrama de secuencia muestra la secuencia de pasos que se producen cuando un cliente realiza un pedido. El diagrama de casos de uso muestra los diferentes casos de uso del sistema y sus flujos principales y alternativos.