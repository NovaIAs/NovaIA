```sql
-- Crear una tabla temporal llamada "VentasAnuales" para resumir las ventas anuales por producto y región.
CREATE TEMP TABLE VentasAnuales AS
SELECT
    strftime('%Y', fecha_venta) AS año_venta,   -- Extraer el año de la fecha de venta como "año_venta".
    producto_id,                               -- Mantener el identificador del producto.
    region_id,                                  -- Mantener el identificador de la región.
    SUM(cantidad_vendida) AS cantidad_vendida_anual  -- Sumar la cantidad vendida para cada año y producto.
FROM
    ventas  -- La tabla original de ventas.
GROUP BY
    año_venta, producto_id, region_id;  -- Agrupar los datos por año de venta, identificación de producto e identificación de región.

-- Crear una tabla temporal llamada "ProductosMásVendidos" para identificar los productos más vendidos en cada región.
CREATE TEMP TABLE ProductosMásVendidos AS
SELECT
    año_venta,
    region_id,
    producto_id,
    cantidad_vendida_anual,
    RANK() OVER (PARTITION BY año_venta, region_id ORDER BY cantidad_vendida_anual DESC) AS ranking_producto
FROM
    VentasAnuales;  -- Utilizar la tabla "VentasAnuales" como fuente de datos.

-- Crear una consulta para recuperar los 10 productos más vendidos en cada región para cada año.
SELECT
    año_venta,
    region_id,
    p.producto_nombre AS producto_nombre,
    cantidad_vendida_anual
FROM
    ProductosMásVendidos pmv
JOIN
    productos p ON pmv.producto_id = p.producto_id  -- Unir la tabla "ProductosMásVendidos" con la tabla "Productos" para obtener el nombre del producto.
WHERE
    ranking_producto <= 10  -- Filtrar por los 10 productos con el ranking más alto.
ORDER BY
    año_venta, region_id, cantidad_vendida_anual DESC;  -- Ordenar los resultados por año, región y cantidad vendida en orden descendente.
```

**Explicación del código:**

* La consulta utiliza varias tablas temporales para realizar cálculos intermedios y filtrar los datos.
* La tabla "VentasAnuales" se crea para resumir las ventas anuales por producto y región.
* La tabla "ProductosMásVendidos" se crea para identificar los productos más vendidos en cada región para cada año.
* La consulta final utiliza una combinación entre "ProductosMásVendidos" y "Productos" para obtener tanto el identificador del producto como el nombre del producto.
* La cláusula "WHERE" se utiliza para filtrar los resultados y solo mostrar los 10 productos más vendidos en cada región para cada año.
* La cláusula "ORDER BY" se utiliza para ordenar los resultados por año, región y cantidad vendida en orden descendente.

Este código es bastante complejo y utiliza varias características avanzadas de SQL, como tablas temporales, funciones de ventana y combinaciones. Sin embargo, proporciona una forma eficiente de identificar los 10 productos más vendidos en cada región para cada año, lo que puede ser útil para analizar tendencias de ventas y tomar decisiones comerciales informadas.