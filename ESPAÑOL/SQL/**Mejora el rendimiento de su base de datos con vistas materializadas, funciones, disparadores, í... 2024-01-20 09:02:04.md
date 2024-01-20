```sql
-- Crear una vista materializada para acelerar las consultas en la tabla "ventas"
CREATE MATERIALIZED VIEW ventas_materializada AS
SELECT
    fecha,
    producto,
    SUM(cantidad) AS cantidad_vendida,
    SUM(precio_unitario * cantidad) AS importe_total
FROM
    ventas
GROUP BY
    fecha,
    producto;

-- Crear una función para calcular el descuento en función del importe total de la compra
CREATE FUNCTION calcular_descuento(@importe_total DECIMAL(10, 2)) RETURNS DECIMAL(10, 2)
AS
BEGIN
    IF @importe_total < 100 THEN
        RETURN 0;
    ELSEIF @importe_total < 200 THEN
        RETURN @importe_total * 0.05;
    ELSE
        RETURN @importe_total * 0.1;
    END IF;
END;

-- Crear un disparador para actualizar el campo "descuento" en la tabla "ventas" cuando se inserta o actualiza un registro
CREATE TRIGGER actualizar_descuento ON ventas
FOR INSERT, UPDATE
AS
BEGIN
    UPDATE ventas
    SET descuento = calcular_descuento(precio_unitario * cantidad)
    WHERE id_venta IN (SELECT id_venta FROM inserted);
END;

-- Crear un índice en la columna "fecha" de la tabla "ventas" para mejorar el rendimiento de las consultas
CREATE INDEX idx_ventas_fecha ON ventas (fecha);

-- Crear un índice agrupado en las columnas "producto" y "fecha" de la tabla "ventas" para optimizar las consultas que agrupan por estas columnas
CREATE CLUSTERED INDEX idx_ventas_producto_fecha ON ventas (producto, fecha);

-- Crear un índice único en la columna "código_producto" de la tabla "productos" para garantizar que cada producto tenga un código único
CREATE UNIQUE INDEX idx_productos_codigo_producto ON productos (código_producto);

-- Crear una vista para mostrar los productos con su stock disponible
CREATE VIEW productos_stock AS
SELECT
    p.id_producto,
    p.nombre,
    p.precio_unitario,
    s.cantidad_stock
FROM
    productos p
JOIN
    stocks s ON p.id_producto = s.id_producto;

-- Crear un procedimiento almacenado para realizar una compra de productos
CREATE PROCEDURE realizar_compra(
    @id_cliente INT,
    @id_producto INT,
    @cantidad INT
)
AS
BEGIN
    DECLARE @precio_unitario DECIMAL(10, 2);
    DECLARE @descuento DECIMAL(10, 2);
    DECLARE @importe_total DECIMAL(10, 2);

    -- Obtener el precio unitario del producto
    SELECT precio_unitario INTO @precio_unitario
    FROM productos
    WHERE id_producto = @id_producto;

    -- Calcular el descuento
    SELECT calcular_descuento(@precio_unitario * @cantidad) INTO @descuento;

    -- Calcular el importe total de la compra
    SELECT (@precio_unitario - @descuento) * @cantidad INTO @importe_total;

    -- Insertar la venta en la tabla "ventas"
    INSERT INTO ventas (id_cliente, id_producto, cantidad, precio_unitario, descuento, importe_total)
    VALUES (@id_cliente, @id_producto, @cantidad, @precio_unitario, @descuento, @importe_total);

    -- Actualizar el stock del producto
    UPDATE stocks
    SET cantidad_stock = cantidad_stock - @cantidad
    WHERE id_producto = @id_producto;
END;
```

Explicación:

Este código SQL es bastante complejo y contiene varias características avanzadas de SQL. Aquí hay una breve explicación de cada parte del código:

1. **Vista Materializada:** Se crea una vista materializada llamada "ventas_materializada" para acelerar las consultas en la tabla "ventas". Esta vista contiene los datos agregados de la tabla "ventas", agrupados por fecha y producto, lo que puede mejorar el rendimiento de las consultas que agrupan por estas columnas.

2. **Función:** Se crea una función llamada "calcular_descuento" que calcula el descuento en función del importe total de la compra. La función utiliza la lógica condicional para aplicar diferentes tasas de descuento en función del importe total.

3. **Disparador:** Se crea un disparador llamado "actualizar_descuento" en la tabla "ventas" que se activa cuando se inserta o actualiza un registro. El disparador utiliza la función "calcular_descuento" para actualizar el campo "descuento" en la tabla "ventas" en función del importe total de la compra.

4. **Índice:** Se crean índices en las columnas "fecha" y "producto" de la tabla "ventas" para mejorar el rendimiento de las consultas que agrupan por estas columnas.

5. **Índice Único:** Se crea un índice único en la columna "código_producto" de la tabla "productos" para garantizar que cada producto tenga un código único.

6. **Vista:** Se crea una vista llamada "productos_stock" que muestra los productos con su stock disponible. La vista combina datos de las tablas "productos" y "stocks".

7. **Procedimiento Almacenado:** Se crea un procedimiento almacenado llamado "realizar_compra" que se utiliza para realizar una compra de productos. El procedimiento almacenado toma varios parámetros, incluyendo el ID del cliente, el ID del producto y la cantidad, y realiza varias operaciones, incluyendo insertar la venta en la tabla "ventas" y actualizar el stock del producto.

Este código SQL es complejo y contiene varias características avanzadas de SQL. Es posible que necesite adaptar el código a su entorno específico y requisitos.