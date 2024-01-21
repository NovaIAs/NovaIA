```sql
-- Crear una tabla temporal para almacenar datos intermedios
CREATE TEMP TABLE TablaIntermedia (
    id_cliente INT,
    nombre_cliente VARCHAR(100),
    id_producto INT,
    nombre_producto VARCHAR(100),
    cantidad_vendida INT,
    precio_unitario NUMERIC(10, 2),
    total_vendido NUMERIC(10, 2)
);

-- Insertar datos en la tabla temporal desde la tabla de ventas
INSERT INTO TablaIntermedia (
    id_cliente,
    nombre_cliente,
    id_producto,
    nombre_producto,
    cantidad_vendida,
    precio_unitario,
    total_vendido
)
SELECT
    s.id_cliente,
    c.nombre_cliente,
    s.id_producto,
    p.nombre_producto,
    s.cantidad_vendida,
    p.precio_unitario,
    s.cantidad_vendida * p.precio_unitario AS total_vendido
FROM
    ventas s
LEFT JOIN
    clientes c ON s.id_cliente = c.id_cliente
LEFT JOIN
    productos p ON s.id_producto = p.id_producto;

-- Crear una tabla final para almacenar los resultados agregados
CREATE TABLE ResultadosFinales (
    id_cliente INT,
    nombre_cliente VARCHAR(100),
    total_vendido NUMERIC(10, 2)
);

-- Insertar datos en la tabla final desde la tabla temporal, agrupando por clientes y calculando el total de ventas
INSERT INTO ResultadosFinales (
    id_cliente,
    nombre_cliente,
    total_vendido
)
SELECT
    id_cliente,
    nombre_cliente,
    SUM(total_vendido) AS total_vendido
FROM
    TablaIntermedia
GROUP BY
    id_cliente,
    nombre_cliente;

-- Eliminar la tabla temporal
DROP TABLE TablaIntermedia;

-- Seleccionar los clientes con ventas totales mayores a un valor especificado
SELECT
    id_cliente,
    nombre_cliente,
    total_vendido
FROM
    ResultadosFinales
WHERE
    total_vendido > 10000;
```

Explicación:

1. Creamos una tabla temporal llamada `TablaIntermedia` para almacenar datos intermedios. Esta tabla contiene información sobre cada venta, incluyendo el ID del cliente, el nombre del cliente, el ID del producto, el nombre del producto, la cantidad vendida, el precio unitario y el total vendido.

2. Insertamos datos en la tabla `TablaIntermedia` desde la tabla de ventas, utilizando una instrucción `INSERT` con una subconsulta. Esta subconsulta selecciona los datos relevantes de las tablas `ventas`, `clientes` y `productos`, y calcula el total vendido para cada venta.

3. Creamos una tabla final llamada `ResultadosFinales` para almacenar los resultados agregados. Esta tabla contiene información sobre cada cliente, incluyendo el ID del cliente, el nombre del cliente y el total de ventas.

4. Insertamos datos en la tabla `ResultadosFinales` desde la tabla `TablaIntermedia`, utilizando una instrucción `INSERT` con una subconsulta. Esta subconsulta agrupa los datos por el ID del cliente y el nombre del cliente, y calcula el total de ventas para cada cliente.

5. Eliminamos la tabla temporal `TablaIntermedia` utilizando la instrucción `DROP TABLE`.

6. Seleccionamos los clientes con ventas totales mayores a un valor especificado utilizando una instrucción `SELECT` con una cláusula `WHERE`. En este caso, seleccionamos los clientes con ventas totales mayores a 10000.