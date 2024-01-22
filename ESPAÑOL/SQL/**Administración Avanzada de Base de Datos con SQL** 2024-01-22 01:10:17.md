```sql
-- Crear una vista materializada (materialized view) llamada "v_ventas_mensuales" que resuma las ventas por mes y producto en la tabla "ventas".

CREATE MATERIALIZED VIEW v_ventas_mensuales AS
SELECT
    strftime('%Y-%m', fecha_venta) AS mes, -- Extraer el año y el mes de la fecha de venta
    producto_id, -- ID del producto
    SUM(cantidad) AS cantidad_vendida, -- Cantidad total vendida del producto en el mes
    SUM(precio_venta * cantidad) AS ventas_totales -- Ingresos totales por la venta del producto en el mes
FROM ventas
GROUP BY 1, 2; -- Agrupar los resultados por mes y producto

-- Crear un índice en la columna "mes" de la vista materializada para acelerar las consultas.

CREATE INDEX idx_v_ventas_mensuales_mes ON v_ventas_mensuales (mes);

-- Crear una función definida por el usuario (UDF) llamada "calcular_descuento" que calcule el descuento aplicado a un precio en función de un porcentaje de descuento.

CREATE FUNCTION calcular_descuento(precio DECIMAL(10, 2), porcentaje_descuento DECIMAL(5, 2))
RETURNS DECIMAL(10, 2)
AS
BEGIN
    RETURN precio * (1 - porcentaje_descuento); -- Calcular el descuento aplicando el porcentaje al precio
END;

-- Crear un disparador (trigger) llamado "trg_control_stock" en la tabla "productos" que actualice automáticamente el stock disponible al insertar o actualizar un registro.

CREATE TRIGGER trg_control_stock
ON productos
FOR INSERT OR UPDATE
AS
BEGIN
    -- Obtener el ID del producto que se está insertando o actualizando.
    DECLARE @producto_id INT;
    SELECT @producto_id = id FROM productos WHERE id = NEW.id;

    -- Obtener el stock disponible actual del producto.
    DECLARE @stock_actual INT;
    SELECT @stock_actual = stock_disponible FROM productos WHERE id = @producto_id;

    -- Si se está insertando un nuevo producto, establecer el stock disponible inicial.
    IF INSERTED.id IS NOT NULL AND INSERTED.stock_disponible IS NULL
    BEGIN
        SET @stock_actual = 0;
    END;

    -- Calcular el nuevo stock disponible en función de la cantidad insertada o actualizada.
    DECLARE @nuevo_stock_disponible INT;
    IF INSERTED.cantidad IS NOT NULL
    BEGIN
        @nuevo_stock_disponible = @stock_actual + INSERTED.cantidad;
    END;
    ELSE
    BEGIN
        @nuevo_stock_disponible = @stock_actual - INSERTED.cantidad;
    END;

    -- Actualizar el stock disponible del producto.
    UPDATE productos SET stock_disponible = @nuevo_stock_disponible WHERE id = @producto_id;
END;

-- Crear una función de ventana (window function) llamada "calcular_promedio_ventas" que calcule el promedio de ventas de los últimos 12 meses para cada producto.

CREATE FUNCTION calcular_promedio_ventas(producto_id INT)
RETURNS DECIMAL(10, 2)
AS
BEGIN
    -- Obtener las ventas de los últimos 12 meses para el producto especificado.
    WITH VentasMensuales AS (
        SELECT
            strftime('%Y-%m', fecha_venta) AS mes,
            SUM(precio_venta * cantidad) AS ventas_totales
        FROM ventas
        WHERE producto_id = producto_id
        AND fecha_venta BETWEEN strftime('%Y-%m-%d %H:%M:%S', date('now', '-12 months')) AND strftime('%Y-%m-%d %H:%M:%S', date('now'))
        GROUP BY 1
    )

    -- Calcular el promedio de las ventas mensuales.
    RETURN AVG(ventas_totales) OVER (ORDER BY mes);
END;
```

Este código SQL es complejo y abarca una variedad de características y funcionalidades. Las siguientes son algunas explicaciones y detalles sobre cada parte del código:

1. **Vista Materializada**:

    * Se crea una vista materializada llamada "v_ventas_mensuales" que resume las ventas por mes y producto en la tabla "ventas".
    * Esto significa que los resultados de esta consulta se almacenan en una tabla física, lo que puede acelerar las consultas que utilizan esta información.

2. **Índice**:

    * Se crea un índice en la columna "mes" de la vista materializada "v_ventas_mensuales" para mejorar el rendimiento de las consultas que filtran por mes.

3. **Función Definida por el Usuario**:

    * Se crea una función definida por el usuario (UDF) llamada "calcular_descuento" que calcula el descuento aplicado a un precio en función de un porcentaje de descuento.
    * Esta función se puede utilizar en consultas para aplicar descuentos a los precios de los productos.

4. **Disparador**:

    * Se crea un disparador llamado "trg_control_stock" en la tabla "productos" que actualiza automáticamente el stock disponible al insertar o actualizar un registro.
    * Esto asegura que el stock disponible siempre esté actualizado y refleje la cantidad real de productos disponibles.

5. **Función de Ventana**:

    * Se crea una función de ventana llamada "calcular_promedio_ventas" que calcula el promedio de ventas de los últimos 12 meses para cada producto.
    * Esta función se puede utilizar en consultas para analizar las tendencias de ventas y tomar decisiones informadas.

Este código abarca una variedad de características avanzadas de SQL y demuestra cómo se pueden combinar diferentes técnicas para crear soluciones complejas y eficientes a problemas reales.