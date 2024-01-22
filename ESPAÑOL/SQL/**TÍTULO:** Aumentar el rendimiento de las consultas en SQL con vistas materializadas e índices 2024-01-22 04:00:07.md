```sql
-- Obtener los 10 clientes con mayor facturación total en el último año

SELECT
    c.nombre AS "Nombre del cliente",
    c.email AS "Correo electrónico",
    SUM(f.total) AS "Facturación total"
FROM
    clientes c
JOIN
    facturas f ON c.id = f.cliente_id
WHERE
    f.fecha BETWEEN DATE('now', '-1 year') AND DATE('now')
GROUP BY
    c.id, c.nombre, c.email
ORDER BY
    "Facturación total" DESC
LIMIT 10;

-- Crear una vista materializada para almacenar los resultados de la consulta anterior

CREATE MATERIALIZED VIEW clientes_con_mayor_facturacion AS
SELECT
    c.nombre AS "Nombre del cliente",
    c.email AS "Correo electrónico",
    SUM(f.total) AS "Facturación total"
FROM
    clientes c
JOIN
    facturas f ON c.id = f.cliente_id
WHERE
    f.fecha BETWEEN DATE('now', '-1 year') AND DATE('now')
GROUP BY
    c.id, c.nombre, c.email;

-- Crear un índice en la vista materializada para mejorar el rendimiento de las consultas

CREATE INDEX clientes_con_mayor_facturacion_idx ON clientes_con_mayor_facturacion ("Facturación total");

-- Utilizar la vista materializada para obtener los 10 clientes con mayor facturación total en el último año (consulta más rápida)

SELECT
    "Nombre del cliente",
    "Correo electrónico",
    "Facturación total"
FROM
    clientes_con_mayor_facturacion
ORDER BY
    "Facturación total" DESC
LIMIT 10;

```

Explicación del código:

1. La primera consulta obtiene los 10 clientes con mayor facturación total en el último año.
2. La segunda consulta crea una vista materializada para almacenar los resultados de la primera consulta. Esto significa que los resultados de la consulta se almacenarán en una tabla física en la base de datos, lo que hará que las consultas futuras a esta vista sean más rápidas.
3. La tercera consulta crea un índice en la vista materializada para mejorar el rendimiento de las consultas. Un índice es una estructura de datos que acelera la búsqueda de datos en una tabla.
4. La cuarta consulta utiliza la vista materializada para obtener los 10 clientes con mayor facturación total en el último año. Esta consulta será más rápida que la primera consulta porque los resultados de la consulta ya están almacenados en una tabla física.