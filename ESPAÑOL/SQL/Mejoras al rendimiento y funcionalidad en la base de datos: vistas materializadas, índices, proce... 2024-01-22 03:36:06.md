```sql
-- Crear una vista materializada para acelerar las consultas en la tabla `pedidos`
CREATE MATERIALIZED VIEW pedidos_materializada AS
SELECT
    pedido_id,
    cliente_id,
    producto_id,
    cantidad,
    precio_unitario,
    total
FROM pedidos

-- Crear un índice en la columna `cliente_id` de la tabla `pedidos` para mejorar el rendimiento de las consultas que filtran por cliente
CREATE INDEX pedidos_cliente_id_idx ON pedidos (cliente_id)

-- Crear un procedimiento almacenado para obtener el total de ventas por producto
CREATE PROCEDURE obtener_total_ventas_por_producto (
    IN producto_id INT,
    OUT total_ventas DECIMAL(10,2)
)
AS
BEGIN
    SELECT SUM(total) INTO total_ventas
    FROM pedidos
    WHERE producto_id = producto_id;
END

-- Crear una función para calcular el descuento de un producto en función de su cantidad
CREATE FUNCTION calcular_descuento (
    cantidad INT
)
RETURNS DECIMAL(10,2)
AS
BEGIN
    IF cantidad < 10 THEN
        RETURN 0.00;
    ELSIF cantidad < 20 THEN
        RETURN 0.05;
    ELSE
        RETURN 0.10;
    END IF;
END

-- Crear un disparador para actualizar el campo `total` de la tabla `pedidos` cuando se modifica el campo `cantidad`
CREATE TRIGGER actualizar_total_pedidos
ON pedidos
FOR UPDATE
AS
BEGIN
    UPDATE pedidos
    SET total = cantidad * precio_unitario
    WHERE pedido_id = NEW.pedido_id;
END
```

Explicación:

* La vista materializada `pedidos_materializada` se crea para mejorar el rendimiento de las consultas que se ejecutan con frecuencia en la tabla `pedidos`.
* El índice `pedidos_cliente_id_idx` se crea en la columna `cliente_id` de la tabla `pedidos` para mejorar el rendimiento de las consultas que filtran por cliente.
* El procedimiento almacenado `obtener_total_ventas_por_producto` se crea para obtener el total de ventas de un producto específico.
* La función `calcular_descuento` se crea para calcular el descuento de un producto en función de su cantidad.
* El disparador `actualizar_total_pedidos` se crea para actualizar el campo `total` de la tabla `pedidos` cuando se modifica el campo `cantidad`.