```smalltalk
Organización: MiEmpresa

Proyecto: Sistema de Facturación

Clase: Factura

Atributos:
- número: Número de factura
- fecha: Fecha de factura
- cliente: Cliente
- productos: Lista de productos facturados
- total: Total de la factura

Métodos:

- #initialize: Constructor de la clase.
- #número: Devuelve el número de factura.
- #fecha: Devuelve la fecha de factura.
- #cliente: Devuelve el cliente de la factura.
- #productos: Devuelve la lista de productos facturados.
- #total: Devuelve el total de la factura.
- #agregarProducto: Agrega un producto a la factura.
- #calcularTotal: Calcula el total de la factura.
- #imprimir: Imprime la factura.


Clase: Cliente

Atributos:
- nombre: Nombre del cliente
- dirección: Dirección del cliente
- teléfono: Teléfono del cliente

Métodos:

- #initialize: Constructor de la clase.
- #nombre: Devuelve el nombre del cliente.
- #dirección: Devuelve la dirección del cliente.
- #teléfono: Devuelve el teléfono del cliente.


Clase: Producto

Atributos:
- nombre: Nombre del producto
- precio: Precio del producto

Métodos:

- #initialize: Constructor de la clase.
- #nombre: Devuelve el nombre del producto.
- #precio: Devuelve el precio del producto.
```

Este código en Smalltalk crea un sistema de facturación básico con las siguientes características:

* Una clase `Factura` que representa una factura, con atributos como el número, la fecha, el cliente, los productos y el total.
* Una clase `Cliente` que representa un cliente, con atributos como el nombre, la dirección y el teléfono.
* Una clase `Producto` que representa un producto, con atributos como el nombre y el precio.

La clase `Factura` tiene métodos para inicializar una factura, devolver el número, la fecha, el cliente, los productos y el total, agregar un producto, calcular el total e imprimir la factura.

La clase `Cliente` tiene métodos para inicializar un cliente, devolver el nombre, la dirección y el teléfono.

La clase `Producto` tiene métodos para inicializar un producto, devolver el nombre y el precio.

El código también incluye un ejemplo de uso de las clases para crear una factura.