```sql
-- Definir una función común de tabla (CTE) llamada UsuariosActivos que devuelva una lista de todos los usuarios activos en el sistema.

WITH UsuariosActivos AS (
    SELECT
        DISTINCT u.id_usuario,
        u.nombre_usuario,
        u.email
    FROM
        usuarios u
    WHERE
        u.estado = 'activo'
),

-- Definir una CTE llamada UsuariosConCompras que devuelva una lista de todos los usuarios que han realizado al menos una compra.

UsuariosConCompras AS (
    SELECT
        DISTINCT u.id_usuario
    FROM
        usuarios u
    INNER JOIN
        compras c ON u.id_usuario = c.id_usuario
),

-- Definir una CTE llamada ProductosVendidos que devuelva una lista de todos los productos que se han vendido al menos una vez.

ProductosVendidos AS (
    SELECT
        DISTINCT p.id_producto,
        p.nombre_producto
    FROM
        productos p
    INNER JOIN
        compras c ON p.id_producto = c.id_producto
)

-- Seleccionar y mostrar:
-- - El nombre de usuario
-- - El número de compras realizadas por ese usuario
-- - El nombre del producto más vendido
-- - El número total de unidades vendidas de ese producto

SELECT
    ua.nombre_usuario,
    COUNT(DISTINCT c.id_compra) AS numero_compras,
    ps.nombre_producto AS producto_mas_vendido,
    SUM(c.cantidad) AS unidades_vendidas
FROM
    UsuariosActivos ua
INNER JOIN
    Compras c ON ua.id_usuario = c.id_usuario
INNER JOIN
    ProductosVendidos ps ON c.id_producto = ps.id_producto
GROUP BY
    ua.id_usuario, ua.nombre_usuario, ps.nombre_producto
ORDER BY
    numero_compras DESC
LIMIT 1;
```

**Explicación:**

1. La primera CTE, `UsuariosActivos`, devuelve una lista de todos los usuarios activos en el sistema. Esto se realiza mediante la selección de los usuarios de la tabla `usuarios` donde el campo `estado` es igual a `activo`.

2. La segunda CTE, `UsuariosConCompras`, devuelve una lista de todos los usuarios que han realizado al menos una compra. Esto se realiza mediante la selección de los usuarios de la tabla `usuarios` que están enlazados a la tabla `compras` mediante el campo `id_usuario`.

3. La tercera CTE, `ProductosVendidos`, devuelve una lista de todos los productos que se han vendido al menos una vez. Esto se realiza mediante la selección de los productos de la tabla `productos` que están enlazados a la tabla `compras` mediante el campo `id_producto`.

4. La consulta principal selecciona y muestra el nombre de usuario, el número de compras realizadas por ese usuario, el nombre del producto más vendido y el número total de unidades vendidas de ese producto. Esto se realiza mediante el uso de uniones internas entre las CTE y la tabla `compras`. La consulta se agrupa por el `id_usuario`, el `nombre_usuario` y el `nombre_producto`, y se ordena por el número de compras en orden descendente. Finalmente, se limita el resultado a una sola fila utilizando la cláusula `LIMIT 1`.