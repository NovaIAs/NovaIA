```
Diagrama de Casos de Uso

Actores:
- Cliente
- Administrador

Casos de Uso:

- Realizar Pedido
- Consultar Estado del Pedido
- Modificar Pedido
- Cancelar Pedido

Diagrama de Clases

Clases:
- Cliente
- Pedido
- Artículo
- Dirección
- Pago

Atributos:
- Cliente: id, nombre, dirección, teléfono, correo electrónico
- Pedido: id, fecha, hora, estado, importe, artículos
- Artículo: id, nombre, descripción, precio
- Dirección: id, calle, número, ciudad, provincia, país
- Pago: id, tipo, importe, fecha

Métodos:
- Cliente: registrarse(), iniciar sesión(), cerrar sesión(), realizar pedido(), consultar estado del pedido(), pagar pedido()
- Pedido: crear(), modificar(), cancelar(), finalizar()
- Artículo: añadir al pedido(), eliminar del pedido()
- Dirección: modificar()
- Pago: realizar()

Diagrama de Secuencia

Caso de Uso: Realizar un pedido

1. El cliente se registra en el sistema
2. El cliente inicia sesión en el sistema
3. El cliente selecciona los artículos que desea comprar
4. El cliente añade los artículos al pedido
5. El cliente introduce la dirección de entrega
6. El cliente selecciona el método de pago
7. El cliente paga el pedido
8. El sistema envía una confirmación del pedido al cliente
9. El pedido se entrega en la dirección indicada por el cliente

Diagrama de Actividades

Caso de Uso: Procesar un pedido

1. El cliente realiza el pedido
2. El sistema recibe el pedido
3. El sistema valida el pedido
4. El sistema envía el pedido al almacén
5. El almacén prepara el pedido
6. El almacén envía el pedido al cliente
7. El cliente recibe el pedido
8. El cliente confirma la recepción del pedido

Diagrama de Estados

Estado del Pedido:

- Nuevo
- Pendiente de pago
- Pagado
- Enviado
- Entregado
- Cancelado

Transiciones:

- Nuevo -> Pendiente de pago: el cliente realiza el pedido
- Pendiente de pago -> Pagado: el cliente paga el pedido
- Pagado -> Enviado: el almacén prepara y envía el pedido
- Enviado -> Entregado: el cliente recibe el pedido
- Cancelado: el cliente cancela el pedido

Diagrama de Paquetes

El sistema se divide en los siguientes paquetes:

- Dominio: contiene las clases que representan el dominio del problema
- Infraestructura: contiene las clases que proporcionan la infraestructura para el sistema
- Interfaz de Usuario: contiene las clases que proporcionan la interfaz de usuario para el sistema

Espero que este código te sea útil.