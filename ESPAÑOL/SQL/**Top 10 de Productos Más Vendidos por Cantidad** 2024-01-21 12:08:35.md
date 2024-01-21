```sql
-- Crear una tabla temporal para almacenar datos intermedios
CREATE TEMP TABLE tmp_tabla_intermedia AS
SELECT
    t1.id_cliente,
    t2.id_producto,
    SUM(t3.cantidad) AS cantidad_vendida
FROM
    tabla_clientes AS t1
JOIN
    tabla_pedidos AS t2 ON t1.id_cliente = t2.id_cliente
JOIN
    tabla_lineas_pedido AS t3 ON t2.id_pedido = t3.id_pedido
GROUP BY t1.id_cliente, t2.id_producto;

-- Obtener el top 10 de productos más vendidos por cantidad
SELECT
    id_producto,
    SUM(cantidad_vendida) AS cantidad_total_vendida
FROM
    tmp_tabla_intermedia
GROUP BY id_producto
ORDER BY cantidad_total_vendida DESC
LIMIT 10;
```

Explicación:

1. La primera consulta crea una tabla temporal llamada `tmp_tabla_intermedia` que almacena los datos intermedios. Esta tabla contiene el ID del cliente, el ID del producto y la cantidad vendida de cada producto por cada cliente.

2. La segunda consulta utiliza la tabla temporal creada en el paso anterior para obtener el top 10 de productos más vendidos por cantidad. Agrupa los datos por el ID del producto y suma la cantidad vendida de cada producto. Luego, ordena los resultados en orden descendente por la cantidad total vendida y limita el resultado a los 10 primeros productos.

Este código es complejo porque utiliza una tabla temporal para almacenar datos intermedios y realiza una serie de operaciones de agrupación y ordenación para obtener el resultado deseado.