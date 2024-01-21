```sql
-- Crear una vista materializada que resume las ventas por producto y región
CREATE MATERIALIZED VIEW vw_ventas_por_producto_region AS
SELECT
    p.nombre_producto,
    r.nombre_region,
    SUM(v.cantidad) AS unidades_vendidas,
    SUM(v.precio_unitario * v.cantidad) AS ventas_totales
FROM
    ventas v
JOIN
    productos p ON v.id_producto = p.id_producto
JOIN
    regiones r ON v.id_region = r.id_region
GROUP BY
    p.nombre_producto,
    r.nombre_region;

-- Crear una función escalar que calcula el descuento basado en la cantidad de artículos comprados
CREATE FUNCTION fn_calcular_descuento(
    @cantidad_articulos INT
)
RETURNS DECIMAL(4, 2)
AS
BEGIN
    DECLARE @descuento DECIMAL(4, 2);

    IF @cantidad_articulos < 10 THEN
        SET @descuento = 0;
    ELSEIF @cantidad_articulos >= 10 AND @cantidad_articulos < 20 THEN
        SET @descuento = 0.1;
    ELSEIF @cantidad_articulos >= 20 AND @cantidad_articulos < 30 THEN
        SET @descuento = 0.15;
    ELSE
        SET @descuento = 0.2;
    END IF;

    RETURN @descuento;
END;

-- Crear un procedimiento almacenado que actualiza el stock de un producto
CREATE PROCEDURE sp_actualizar_stock(
    @id_producto INT,
    @cantidad_vendida INT
)
AS
BEGIN
    UPDATE
        productos
    SET
        stock = stock - @cantidad_vendida
    WHERE
        id_producto = @id_producto;
END;

-- Crear un disparador que registra los cambios en la tabla de ventas
CREATE TRIGGER tr_ventas_audit ON ventas
FOR INSERT, UPDATE, DELETE
AS
BEGIN
    INSERT INTO
        ventas_audit (id_venta, fecha, operacion, usuario)
    VALUES (
        NEW.id_venta,
        GETDATE(),
        CASE
            WHEN INSERTED.id_venta IS NOT NULL THEN 'INSERT'
            WHEN DELETED.id_venta IS NOT NULL THEN 'DELETE'
            ELSE 'UPDATE'
        END,
        CURRENT_USER
    );
END;
```

Explicación del código:

* La vista materializada `vw_ventas_por_producto_region` resume las ventas por producto y región, lo que permite consultas rápidas sobre estos datos.
* La función escalar `fn_calcular_descuento` calcula el descuento basado en la cantidad de artículos comprados.
* El procedimiento almacenado `sp_actualizar_stock` actualiza el stock de un producto después de una venta.
* El disparador `tr_ventas_audit` registra los cambios en la tabla de ventas, incluyendo la fecha, la operación realizada y el usuario que la realizó.