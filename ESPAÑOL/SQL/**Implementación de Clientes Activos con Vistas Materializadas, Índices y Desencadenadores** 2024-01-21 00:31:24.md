```sql
-- Este código crea una vista materializada llamada "v_clientes_activos" que contiene los clientes que han realizado al menos una compra en los últimos 30 días.

CREATE MATERIALIZED VIEW v_clientes_activos AS
SELECT
    c.id_cliente,
    c.nombre,
    c.correo_electronico,
    c.telefono,
    MAX(p.fecha_compra) AS ultima_compra
FROM
    clientes c
LEFT JOIN
    compras p ON c.id_cliente = p.id_cliente
WHERE
    p.fecha_compra >= DATE('now', '-30 days')
GROUP BY
    c.id_cliente,
    c.nombre,
    c.correo_electronico,
    c.telefono;

-- Este código crea un índice en la columna "ultima_compra" de la vista materializada "v_clientes_activos" para acelerar las consultas que utilizan esta columna.

CREATE INDEX idx_v_clientes_activos_ultima_compra ON v_clientes_activos (ultima_compra);

-- Este código crea un desencadenador en la tabla "compras" que actualiza la vista materializada "v_clientes_activos" cada vez que se inserta, actualiza o elimina una fila en la tabla "compras".

CREATE TRIGGER trg_compras_update ON compras
AFTER INSERT OR UPDATE OR DELETE
AS
BEGIN
    -- Actualizar la vista materializada "v_clientes_activos" para reflejar los cambios realizados en la tabla "compras".

    REFRESH MATERIALIZED VIEW v_clientes_activos;
END;
```

**Explicación del código:**

1. El primer código crea una vista materializada llamada "v_clientes_activos" utilizando la instrucción `CREATE MATERIALIZED VIEW`. Esta vista materializada contiene los clientes que han realizado al menos una compra en los últimos 30 días.

2. El segundo código crea un índice en la columna "ultima_compra" de la vista materializada "v_clientes_activos" utilizando la instrucción `CREATE INDEX`. Este índice acelerará las consultas que utilizan la columna "ultima_compra".

3. El tercer código crea un desencadenador en la tabla "compras" utilizando la instrucción `CREATE TRIGGER`. Este disparador actualizará la vista materializada "v_clientes_activos" cada vez que se inserte, actualice o elimine una fila en la tabla "compras".

Este código es complejo porque crea una vista materializada, un índice y un desencadenador. Es poco probable que este código se repita porque es específico para la tarea de mantener una vista materializada actualizada que contiene los clientes que han realizado al menos una compra en los últimos 30 días.