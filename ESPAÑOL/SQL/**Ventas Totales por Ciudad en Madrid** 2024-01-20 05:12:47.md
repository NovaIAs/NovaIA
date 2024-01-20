```sql
-- Crear una tabla temporal para almacenar datos intermedios
CREATE TEMP TABLE tmp_tabla AS (
    -- Seleccionar columnas específicas de la tabla original
    SELECT id_cliente, nombre_cliente, ciudad_cliente, monto_compra
    FROM tabla_original
    WHERE ciudad_cliente = 'Madrid'
);

-- Crear una tabla temporal para almacenar datos agregados
CREATE TEMP TABLE tmp_tabla_agregada AS (
    -- Agrupar los datos por ciudad del cliente y calcular el monto total de las compras
    SELECT ciudad_cliente, SUM(monto_compra) AS monto_total_compras
    FROM tmp_tabla
    GROUP BY ciudad_cliente
);

-- Seleccionar los datos de la tabla temporal agregada y ordenarlos por monto total de compras en orden descendente
SELECT ciudad_cliente, monto_total_compras
FROM tmp_tabla_agregada
ORDER BY monto_total_compras DESC;

-- Eliminar las tablas temporales
DROP TABLE tmp_tabla;
DROP TABLE tmp_tabla_agregada;
```

Explicación:

1. Se crea una tabla temporal llamada `tmp_tabla` que almacena los datos de la tabla original `tabla_original` filtrados por la ciudad del cliente igual a 'Madrid'.

2. Se crea una segunda tabla temporal llamada `tmp_tabla_agregada` que agrupa los datos de `tmp_tabla` por ciudad del cliente y calcula el monto total de las compras.

3. Se seleccionan los datos de `tmp_tabla_agregada` y se ordenan por monto total de compras en orden descendente.

4. Finalmente, se eliminan las tablas temporales `tmp_tabla` y `tmp_tabla_agregada`.

Este código es complejo porque utiliza múltiples tablas temporales y consultas para procesar y agregar datos de manera eficiente y organizada. También utiliza funciones de agregación como `SUM()` y `GROUP BY` para calcular el monto total de las compras por ciudad del cliente.