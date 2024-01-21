```sql
-- Crear una vista materializada para acelerar las consultas
CREATE MATERIALIZED VIEW vw_ventas_mensuales AS
SELECT
    strftime('%Y-%m', fecha) AS mes,
    SUM(total) AS ventas_totales
FROM ventas
GROUP BY mes;

-- Crear un índice en la columna "nombre" de la tabla "clientes" para mejorar el rendimiento de las consultas
CREATE INDEX idx_clientes_nombre ON clientes (nombre);

-- Crear un disparador "after insert" en la tabla "pedidos" para actualizar automáticamente el campo "total"
CREATE TRIGGER trg_pedidos_after_insert
AFTER INSERT ON pedidos
FOR EACH ROW
BEGIN
    -- Calcular el total del pedido
    UPDATE pedidos
    SET total = (SELECT SUM(precio * cantidad) FROM detalles_pedido WHERE id_pedido = NEW.id_pedido)
    WHERE id_pedido = NEW.id_pedido;
END;

-- Crear una función definida por el usuario (UDF) para calcular el descuento en función del importe total
CREATE FUNCTION udf_calcular_descuento(total NUMERIC) RETURNS NUMERIC
BEGIN
    CASE
        WHEN total < 100 THEN 0.05
        WHEN total >= 100 AND total < 200 THEN 0.10
        ELSE 0.15
    END;
END;

-- Crear un procedimiento almacenado para insertar un nuevo cliente y devolver su ID
CREATE PROCEDURE sp_insertar_cliente(
    @nombre VARCHAR(50),
    @direccion VARCHAR(100),
    @telefono VARCHAR(20),
    @correo_electronico VARCHAR(50),
    @id_cliente OUT INT
)
AS
BEGIN
    -- Insertar el nuevo cliente
    INSERT INTO clientes (nombre, direccion, telefono, correo_electronico)
    VALUES (@nombre, @direccion, @telefono, @correo_electronico);

    -- Obtener el ID del nuevo cliente
    SELECT last_insert_rowid() INTO @id_cliente;
END;

-- Crear una vista para mostrar los detalles de los pedidos junto con el nombre del cliente
CREATE VIEW vw_pedidos_detalles AS
SELECT
    p.id_pedido,
    p.fecha,
    c.nombre AS nombre_cliente,
    dp.id_producto,
    dp.cantidad,
    dp.precio,
    dp.total
FROM pedidos p
INNER JOIN clientes c ON p.id_cliente = c.id_cliente
INNER JOIN detalles_pedido dp ON p.id_pedido = dp.id_pedido;
```

Explicación del código:

1. **Vista Materializada:** Se crea una vista materializada llamada `vw_ventas_mensuales` para acelerar las consultas que agrupan las ventas por mes. Esto mejora el rendimiento de las consultas que necesitan acceder a datos históricos de ventas.

2. **Índice:** Se crea un índice en la columna "nombre" de la tabla "clientes" para mejorar el rendimiento de las consultas que buscan clientes por su nombre. Esto acelera la búsqueda de clientes en la tabla.

3. **Disparador:** Se crea un disparador "after insert" en la tabla "pedidos" para actualizar automáticamente el campo "total" cada vez que se inserta un nuevo pedido. Esto garantiza que el campo "total" siempre esté actualizado con el valor correcto.

4. **Función Definida por el Usuario (UDF):** Se crea una UDF llamada `udf_calcular_descuento` para calcular el descuento en función del importe total de un pedido. Esto permite reutilizar esta lógica en diferentes partes del código.

5. **Procedimiento Almacenado:** Se crea un procedimiento almacenado llamado `sp_insertar_cliente` para insertar un nuevo cliente en la tabla "clientes" y devolver su ID. Esto facilita la inserción de nuevos clientes en la base de datos.

6. **Vista:** Se crea una vista llamada `vw_pedidos_detalles` para mostrar los detalles de los pedidos junto con el nombre del cliente. Esto simplifica las consultas que necesitan acceder a los detalles de los pedidos y el nombre del cliente.