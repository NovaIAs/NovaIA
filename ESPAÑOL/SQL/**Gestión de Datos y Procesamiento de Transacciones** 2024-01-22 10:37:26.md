```sql
-- Crear una vista materializada para optimizar las consultas sobre datos históricos
CREATE MATERIALIZED VIEW vw_historial_ventas AS
SELECT
    producto_id,
    SUM(cantidad) AS cantidad_vendida,
    SUM(precio_unitario * cantidad) AS importe_total
FROM ventas
WHERE fecha_venta BETWEEN '2021-01-01' AND '2021-12-31'
GROUP BY producto_id;

-- Crear una función escalar para calcular el descuento en función del importe total de la compra
CREATE FUNCTION obtener_descuento(importe_total DECIMAL(10, 2)) RETURNS DECIMAL(10, 2)
AS $$
    CASE
        WHEN importe_total < 100 THEN 0.05 -- 5% de descuento para compras menores a 100
        WHEN importe_total >= 100 AND importe_total < 200 THEN 0.10 -- 10% de descuento para compras entre 100 y 200
        ELSE 0.15 -- 15% de descuento para compras mayores o iguales a 200
    END;
$$ LANGUAGE SQL;

-- Crear un procedimiento almacenado para procesar los pedidos de los clientes
CREATE PROCEDURE procesar_pedido(pedido_id INTEGER)
AS $$
BEGIN
    -- Actualizar el estado del pedido a "Procesado"
    UPDATE pedidos
    SET estado = 'Procesado'
    WHERE pedido_id = pedido_id;

    -- Obtener los detalles del pedido
    SELECT
        producto_id,
        cantidad,
        precio_unitario
    FROM pedidos_detalles
    WHERE pedido_id = pedido_id;

    -- Insertar los detalles del pedido en la tabla de ventas
    INSERT INTO ventas (producto_id, cantidad, precio_unitario, fecha_venta)
    VALUES
        (producto_id, cantidad, precio_unitario, NOW());

    -- Actualizar el stock de los productos vendidos
    UPDATE productos
    SET stock = stock - cantidad
    WHERE producto_id IN (SELECT producto_id FROM pedidos_detalles WHERE pedido_id = pedido_id);
END;
$$ LANGUAGE PL/pgSQL;

-- Crear un disparador para registrar los cambios en la tabla de productos
CREATE TRIGGER log_cambios_productos ON productos
FOR INSERT OR UPDATE OR DELETE
AS $$
BEGIN
    -- Obtener el usuario que realizó el cambio
    SELECT current_user INTO usuario;

    -- Insertar una entrada en la tabla de auditoría de cambios
    INSERT INTO auditoria_cambios_productos (usuario, operacion, fecha_cambio, producto_id, producto_nombre, producto_precio, producto_stock)
    VALUES (
        usuario,
        CASE
            WHEN pg_trigger.op = 'INSERT' THEN 'Inserción'
            WHEN pg_trigger.op = 'UPDATE' THEN 'Actualización'
            WHEN pg_trigger.op = 'DELETE' THEN 'Eliminación'
        END,
        NOW(),
        COALESCE(NEW.producto_id, OLD.producto_id),
        COALESCE(NEW.producto_nombre, OLD.producto_nombre),
        COALESCE(NEW.producto_precio, OLD.producto_precio),
        COALESCE(NEW.producto_stock, OLD.producto_stock)
    );

    -- Devolver TRUE para que el disparador no cancele la operación original
    RETURN TRUE;
END;
$$ LANGUAGE PL/pgSQL;
```

Explicación del código:

1. **Vista materializada:** Se crea una vista materializada para almacenar los datos históricos de ventas del año 2021, lo que permite optimizar las consultas sobre estos datos.

2. **Función escalar:** Se crea una función escalar que calcula el descuento en función del importe total de la compra. Esta función se puede utilizar, por ejemplo, en las consultas de precios para mostrar el precio con descuento.

3. **Procedimiento almacenado:** Se crea un procedimiento almacenado para procesar los pedidos de los clientes. Este procedimiento se encarga de actualizar el estado del pedido, insertar los detalles del pedido en la tabla de ventas y actualizar el stock de los productos vendidos.

4. **Disparador:** Se crea un disparador en la tabla de productos para registrar los cambios realizados en esta tabla. El disparador inserta una entrada en la tabla de auditoría de cambios de productos cada vez que se inserta, actualiza o elimina un producto.