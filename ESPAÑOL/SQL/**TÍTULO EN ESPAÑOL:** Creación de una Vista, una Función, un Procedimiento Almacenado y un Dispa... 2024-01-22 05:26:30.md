```sql
-- Crear una vista que muestre el nombre del cliente, el nombre del producto, el precio del producto y la cantidad pedida para cada pedido.
CREATE VIEW vw_Pedidos AS
SELECT 
    c.nombre AS NombreCliente,
    p.nombre AS NombreProducto,
    p.precio AS PrecioProducto,
    od.cantidad AS CantidadPedida
FROM 
    clientes c
INNER JOIN 
    pedidos p ON c.id_cliente = p.id_cliente
INNER JOIN 
    pedidos_detalles od ON p.id_pedido = od.id_pedido;

-- Crear una función que calcule el descuento para un pedido dado. El descuento se calcula en función del monto total del pedido y se aplica según las siguientes reglas:
-- Si el monto del pedido es mayor o igual a $500, el descuento es del 10%.
-- Si el monto del pedido es mayor o igual a $1000, el descuento es del 15%.
-- Si el monto del pedido es mayor o igual a $2000, el descuento es del 20%.

-- Caso contrario, el descuento es del 5%.
CREATE FUNCTION fn_CalcularDescuento(@MontoTotal DECIMAL(18,2))
RETURNS DECIMAL(18,2) AS
BEGIN
    DECLARE @Descuento DECIMAL(18,2);
    
    IF @MontoTotal >= 500 THEN
        SET @Descuento = 0.10;
    ELSEIF @MontoTotal >= 1000 THEN
        SET @Descuento = 0.15;
    ELSEIF @MontoTotal >= 2000 THEN
        SET @Descuento = 0.20;
    ELSE
        SET @Descuento = 0.05;
    END IF;
    
    RETURN @Descuento;
END;

-- Crear un procedimiento almacenado que actualice el estado de un pedido a "Envíado" cuando se haya procesado el pago del mismo.
CREATE PROCEDURE sp_ActualizarEstadoPedido
(@IdPedido INT)
AS
BEGIN
    UPDATE pedidos
    SET estado = 'Envíado'
    WHERE id_pedido = @IdPedido;
END;

-- Crear un disparador que se ejecute después de insertar un nuevo registro en la tabla "pedidos" y que llame al procedimiento almacenado "sp_ActualizarEstadoPedido" para actualizar el estado del pedido a "Procesado".
CREATE TRIGGER tr_ActualizarEstadoPedido
ON pedidos
AFTER INSERT
AS
BEGIN
    EXEC sp_ActualizarEstadoPedido @IdPedido = last_insert_rowid();
END;
```

**Explicación:**

Este código SQL crea una vista, una función, un procedimiento almacenado y un disparador.

**La vista** llamada `vw_Pedidos` muestra el nombre del cliente, el nombre del producto, el precio del producto y la cantidad pedida para cada pedido. Esto nos permite consultar esta información de manera más fácil y eficiente.

**La función** llamada `fn_CalcularDescuento` calcula el descuento para un pedido dado en función del monto total del mismo. Esta función puede ser utilizada en consultas para aplicar el descuento correspondiente a cada pedido.

**El procedimiento almacenado** llamado `sp_ActualizarEstadoPedido` actualiza el estado de un pedido a "Envíado" cuando se haya procesado el pago del mismo. Este procedimiento puede ser llamado desde una aplicación o desde otra parte del código SQL.

**El disparador** llamado `tr_ActualizarEstadoPedido` se ejecuta después de insertar un nuevo registro en la tabla "pedidos" y llama al procedimiento almacenado `sp_ActualizarEstadoPedido` para actualizar el estado del pedido a "Procesado". Esto garantiza que el estado del pedido se actualice automáticamente cuando se inserte un nuevo registro en la tabla "pedidos".