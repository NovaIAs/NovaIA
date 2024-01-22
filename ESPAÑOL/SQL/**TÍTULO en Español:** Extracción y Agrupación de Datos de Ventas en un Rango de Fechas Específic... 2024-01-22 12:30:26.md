```sql
-- Crear una tabla temporal para almacenar datos intermedios
CREATE TEMP TABLE tmp_tabla AS (
    -- Seleccionar columnas específicas de la tabla original
    SELECT
        id_cliente,
        nombre_cliente,
        fecha_compra,
        producto_comprado,
        cantidad_comprada,
        precio_unitario
    FROM
        tabla_ventas
);

-- Crear una tabla temporal para almacenar datos agregados
CREATE TEMP TABLE tmp_tabla_agregada AS (
    -- Agrupar datos por cliente y producto
    SELECT
        id_cliente,
        producto_comprado,
        SUM(cantidad_comprada) AS cantidad_total_comprada,
        SUM(precio_unitario * cantidad_comprada) AS importe_total_comprado
    FROM
        tmp_tabla
    GROUP BY
        id_cliente,
        producto_comprado
);

-- Crear una tabla temporal para almacenar datos con rangos de fechas
CREATE TEMP TABLE tmp_tabla_fechas AS (
    -- Seleccionar filas que cumplan con un rango de fechas
    SELECT
        id_cliente,
        producto_comprado,
        cantidad_total_comprada,
        importe_total_comprado,
        fecha_compra
    FROM
        tmp_tabla_agregada
    WHERE
        fecha_compra BETWEEN '2023-01-01' AND '2023-12-31'
);

-- Crear la tabla final con los resultados
CREATE TABLE tabla_resultados AS (
    -- Seleccionar datos de la tabla temporal con rangos de fechas
    SELECT
        id_cliente,
        producto_comprado,
        cantidad_total_comprada,
        importe_total_comprado
    FROM
        tmp_tabla_fechas
);

-- Eliminar las tablas temporales
DROP TABLE tmp_tabla;
DROP TABLE tmp_tabla_agregada;
DROP TABLE tmp_tabla_fechas;
```

**Explicación:**

1. **Crear tablas temporales:**

    - `tmp_tabla`: Almacena datos intermedios de la tabla original `tabla_ventas`.
    - `tmp_tabla_agregada`: Almacena datos agregados por cliente y producto.
    - `tmp_tabla_fechas`: Almacena datos con rangos de fechas.

2. **Seleccionar datos específicos:**

    - La consulta extrae columnas específicas de la tabla original, como el ID del cliente, el nombre del cliente, la fecha de compra, el producto comprado, la cantidad comprada y el precio unitario.

3. **Agrupar datos:**

    - Los datos se agrupan por cliente y producto para obtener la cantidad total comprada y el importe total comprado para cada combinación de cliente y producto.

4. **Filtrar datos por rango de fechas:**

    - Los datos se filtran para incluir solo registros dentro de un rango de fechas específico (`'2023-01-01'` y `'2023-12-31'`).

5. **Crear la tabla final:**

    - La tabla `tabla_resultados` se crea como una copia de `tmp_tabla_fechas`, que contiene datos filtrados con rangos de fechas específicos.

6. **Eliminar tablas temporales:**

    - Finalmente, se eliminan las tablas temporales para liberar recursos.