```sql
-- Crear una vista que muestre los detalles de los pedidos y los clientes
CREATE VIEW vw_PedidosClientes AS
SELECT
    p.PedidoID,
    p.FechaPedido,
    p.FechaEntrega,
    p.EstadoPedido,
    c.ClienteID,
    c.NombreCliente,
    c.DirecciónCliente,
    c.CiudadCliente,
    c.EstadoCliente,
    c.CódigoPostalCliente,
    c.TeléfonoCliente,
    c.CorreoElectrónicoCliente
FROM
    Pedidos AS p
INNER JOIN
    Clientes AS c
ON
    p.ClienteID = c.ClienteID;

-- Crear una función que calcule el descuento total para un pedido
CREATE FUNCTION fn_CalcularDescuentoTotal(@PedidoID int)
RETURNS money
AS
BEGIN
    DECLARE @DescuentoTotal money;
    DECLARE @DescuentoPorcentaje decimal(18, 2);
    DECLARE @Subtotal money;

    -- Obtener el subtotal del pedido
    SELECT @Subtotal = Subtotal FROM Pedidos WHERE PedidoID = @PedidoID;

    -- Obtener el porcentaje de descuento del pedido
    SELECT @DescuentoPorcentaje = DescuentoPorcentaje FROM Pedidos WHERE PedidoID = @PedidoID;

    -- Calcular el descuento total aplicando el porcentaje de descuento al subtotal
    SET @DescuentoTotal = @Subtotal * @DescuentoPorcentaje;

    -- Devolver el descuento total
    RETURN @DescuentoTotal;
END;

-- Crear un procedimiento almacenado que actualice el estado de un pedido
CREATE PROCEDURE sp_ActualizarEstadoPedido
(
    @PedidoID int,
    @EstadoPedido varchar(20)
)
AS
BEGIN
    -- Actualizar el estado del pedido
    UPDATE Pedidos
    SET EstadoPedido = @EstadoPedido
    WHERE PedidoID = @PedidoID;
END;

-- Crear un disparador que registre los cambios en la tabla Pedidos
CREATE TRIGGER trg_Pedidos_Cambios
ON Pedidos
FOR INSERT, UPDATE, DELETE
AS
BEGIN
    -- Insertar un registro en la tabla Auditoría de Pedidos con los detalles del cambio
    INSERT INTO Auditoría de Pedidos (PedidoID, FechaCambio, TipoCambio, UsuarioCambio)
    VALUES (
        inserted.PedidoID,
        GETDATE(),
        CASE
            WHEN inserted.PedidoID IS NOT NULL THEN 'Inserción'
            WHEN deleted.PedidoID IS NOT NULL THEN 'Eliminación'
            ELSE 'Actualización'
        END,
        SYSTEM_USER
    );
END;
```

Este código crea una vista, una función, un procedimiento almacenado y un disparador en una base de datos SQL.

La vista **vw_PedidosClientes** muestra los detalles de los pedidos y los clientes. Esta vista se crea utilizando una combinación interna entre las tablas **Pedidos** y **Clientes**.

La función **fn_CalcularDescuentoTotal** calcula el descuento total para un pedido. Esta función toma el ID del pedido como parámetro y devuelve el descuento total. La función utiliza la columna **Subtotal** de la tabla **Pedidos** para calcular el descuento total.

El procedimiento almacenado **sp_ActualizarEstadoPedido** actualiza el estado de un pedido. Este procedimiento almacenado toma el ID del pedido y el nuevo estado del pedido como parámetros. El procedimiento almacenado utiliza la columna **EstadoPedido** de la tabla **Pedidos** para actualizar el estado del pedido.

El disparador **trg_Pedidos_Cambios** registra los cambios en la tabla **Pedidos**. Este disparador se activa cuando se inserta, actualiza o elimina un registro en la tabla **Pedidos**. El disparador utiliza la tabla **Auditoría de Pedidos** para registrar los detalles del cambio.