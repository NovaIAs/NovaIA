```sql
-- Crear una vista que muestre los nombres de los empleados y la información de sus departamentos y gerentes
CREATE VIEW EmpleadosDepartamentos AS
SELECT
    e.nombre_empleado,
    d.nombre_departamento,
    g.nombre_gerente
FROM
    empleados e
LEFT JOIN
    departamentos d ON e.id_departamento = d.id_departamento
LEFT JOIN
    gerentes g ON d.id_gerente = g.id_gerente;

-- Crear una tabla temporal para almacenar los totales de ventas por producto
CREATE TEMP TABLE TotalesVentasProducto AS
SELECT
    p.nombre_producto,
    SUM(s.cantidad_vendida) AS total_vendido
FROM
    productos p
LEFT JOIN
    ventas s ON p.id_producto = s.id_producto
GROUP BY
    p.nombre_producto;

-- Crear una función escalar para calcular el descuento de un cliente según su nivel de fidelidad
CREATE FUNCTION DescuentoCliente(@nivel_fidelidad INT) RETURNS DECIMAL(5, 2)
AS
BEGIN
    IF @nivel_fidelidad < 5 THEN
        RETURN 0.05;
    ELSEIF @nivel_fidelidad >= 5 AND @nivel_fidelidad < 10 THEN
        RETURN 0.10;
    ELSE
        RETURN 0.15;
    END IF;
END;

-- Crear un procedimiento almacenado para procesar pedidos de clientes
CREATE PROCEDURE ProcesarPedido(@id_cliente INT, @id_producto INT, @cantidad INT)
AS
BEGIN
    -- Actualizar el stock del producto
    UPDATE productos
    SET stock = stock - @cantidad
    WHERE id_producto = @id_producto;

    -- Insertar una nueva venta en la tabla de ventas
    INSERT INTO ventas (id_cliente, id_producto, cantidad_vendida)
    VALUES (@id_cliente, @id_producto, @cantidad);

    -- Calcular el total de la venta
    DECLARE @total_venta DECIMAL(10, 2);
    SELECT @total_venta = precio_unitario * @cantidad
    FROM productos
    WHERE id_producto = @id_producto;

    -- Aplicar el descuento al total de la venta
    DECLARE @descuento DECIMAL(5, 2);
    SELECT @descuento = DescuentoCliente(c.nivel_fidelidad)
    FROM clientes c
    WHERE c.id_cliente = @id_cliente;

    @total_venta = @total_venta - (@total_venta * @descuento);

    -- Actualizar el saldo del cliente
    UPDATE clientes
    SET saldo = saldo + @total_venta
    WHERE id_cliente = @id_cliente;
END;

-- Crear un disparador para registrar cambios en la tabla de empleados
CREATE TRIGGER AuditoriaEmpleados
ON Empleados
FOR INSERT, UPDATE, DELETE
AS
BEGIN
    INSERT INTO AuditoriaEmpleados (fecha_cambio, tipo_cambio, nombre_empleado)
    VALUES (GETDATE(), INSERTED.nombre_empleado, 'INSERT');
END;
```

Explicación:

* La vista `EmpleadosDepartamentos` muestra los nombres de los empleados y la información de sus departamentos y gerentes.
* La tabla temporal `TotalesVentasProducto` almacena los totales de ventas por producto.
* La función escalar `DescuentoCliente` calcula el descuento de un cliente según su nivel de fidelidad.
* El procedimiento almacenado `ProcesarPedido` procesa los pedidos de los clientes.
* El disparador `AuditoriaEmpleados` registra los cambios en la tabla `Empleados`.