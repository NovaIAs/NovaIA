```sql
-- Crear una vista que muestre los detalles de los empleados y sus departamentos

CREATE VIEW EmpleadosDepartamentos AS
SELECT
    e.id_empleado,
    e.nombre,
    e.apellido,
    e.fecha_nacimiento,
    e.fecha_contratacion,
    e.salario,
    d.id_departamento,
    d.nombre_departamento
FROM
    empleados e
INNER JOIN
    departamentos d
ON
    e.id_departamento = d.id_departamento;

-- Crear una función que calcule el salario anual de un empleado

CREATE FUNCTION SalarioAnual(@id_empleado INT)
RETURNS MONEY
BEGIN
    -- Obtener el salario del empleado
    DECLARE @salario MONEY;
    SELECT @salario = salario FROM empleados WHERE id_empleado = @id_empleado;

    -- Calcular el salario anual multiplicando el salario mensual por 12
    RETURN @salario * 12;
END;

-- Crear un procedimiento almacenado que aumente el salario de un empleado

CREATE PROCEDURE AumentarSalario(@id_empleado INT, @porcentaje_aumento DECIMAL(5, 2))
AS
BEGIN
    -- Obtener el salario actual del empleado
    DECLARE @salario_actual MONEY;
    SELECT @salario_actual = salario FROM empleados WHERE id_empleado = @id_empleado;

    -- Calcular el nuevo salario aumentando el salario actual por el porcentaje especificado
    DECLARE @nuevo_salario MONEY;
    SET @nuevo_salario = @salario_actual * (1 + @porcentaje_aumento);

    -- Actualizar el salario del empleado en la tabla
    UPDATE empleados SET salario = @nuevo_salario WHERE id_empleado = @id_empleado;
END;

-- Crear un disparador que registre los cambios en la tabla de empleados

CREATE TRIGGER Empleados_Cambios
ON
    empleados
FOR
    INSERT, UPDATE, DELETE
AS
BEGIN
    -- Insertar una fila en la tabla de auditoría con los detalles del cambio
    INSERT INTO auditoria_empleados (id_empleado, operacion, fecha_hora)
    VALUES (
        id_empleado,
        CASE
            WHEN INSERTED.id_empleado IS NOT NULL THEN 'INSERT'
            WHEN DELETED.id_empleado IS NOT NULL THEN 'DELETE'
            ELSE 'UPDATE'
        END,
        GETDATE()
    );
END;

-- Crear una vista que muestre los detalles de los productos y sus categorías

CREATE VIEW ProductosCategorias AS
SELECT
    p.id_producto,
    p.nombre_producto,
    p.precio,
    p.stock,
    c.id_categoria,
    c.nombre_categoria
FROM
    productos p
INNER JOIN
    categorias c
ON
    p.id_categoria = c.id_categoria;

-- Crear una función que calcule el precio total de una venta

CREATE FUNCTION PrecioTotalVenta(@id_venta INT)
RETURNS MONEY
BEGIN
    -- Obtener los detalles de la venta
    DECLARE @venta TABLE (
        id_producto INT,
        cantidad INT,
        precio_unitario MONEY
    );
    INSERT INTO @venta (id_producto, cantidad, precio_unitario)
    SELECT
        dv.id_producto,
        dv.cantidad,
        p.precio
    FROM
        ventas_detalles dv
    INNER JOIN
        productos p
    ON
        dv.id_producto = p.id_producto
    WHERE
        dv.id_venta = @id_venta;

    -- Calcular el precio total de la venta sumando el precio de cada producto multiplicado por su cantidad
    DECLARE @precio_total MONEY;
    SELECT @precio_total = SUM(cantidad * precio_unitario) FROM @venta;

    -- Devolver el precio total de la venta
    RETURN @precio_total;
END;

-- Crear un procedimiento almacenado que genere una factura para una venta

CREATE PROCEDURE GenerarFactura(@id_venta INT)
AS
BEGIN
    -- Obtener los detalles de la venta
    DECLARE @venta TABLE (
        id_producto INT,
        cantidad INT,
        precio_unitario MONEY
    );
    INSERT INTO @venta (id_producto, cantidad, precio_unitario)
    SELECT
        dv.id_producto,
        dv.cantidad,
        p.precio
    FROM
        ventas_detalles dv
    INNER JOIN
        productos p
    ON
        dv.id_producto = p.id_producto
    WHERE
        dv.id_venta = @id_venta;

    -- Obtener los detalles del cliente
    DECLARE @cliente TABLE (
        id_cliente INT,
        nombre_cliente VARCHAR(255),
        direccion_cliente VARCHAR(255)
    );
    INSERT INTO @cliente (id_cliente, nombre_cliente, direccion_cliente)
    SELECT
        c.id_cliente,
        c.nombre_cliente,
        c.direccion_cliente
    FROM
        clientes c
    WHERE
        c.id_cliente = (SELECT id_cliente FROM ventas WHERE id_venta = @id_venta);

    -- Crear la factura en formato HTML
    DECLARE @factura VARCHAR(MAX);
    SET @factura = '<html><body><h1>Factura</h1><table border="1">';
    SET @factura = @factura + '<tr><th>Producto</th><th>Cantidad</th><th>Precio Unitario</th><th>Precio Total</th></tr>';
    SELECT
        @factura = @factura + '<tr><td>' + p.nombre_producto + '</td><td>' + CAST(cantidad AS VARCHAR(255)) + '</td><td>' + CAST(precio_unitario AS VARCHAR(255)) + '</td><td>' + CAST((cantidad * precio_unitario) AS VARCHAR(255)) + '</td></tr>'
    FROM
        @venta;
    SET @factura = @factura + '</table><br>';
    SET @factura = @factura + '<h3>Cliente: ' + nombre_cliente + '</h3>';
    SET @factura = @factura + '<h3>Dirección: ' + direccion_cliente + '</h3>';
    SET @factura = @factura + '<h3>Precio Total: ' + CAST(PrecioTotalVenta(@id_venta) AS VARCHAR(255)) + '</h3>';
    SET @factura = @factura + '</body></html>';

    -- Enviar la factura al cliente por correo electrónico
    EXEC msdb.dbo.sp_send_dbmail
        @profile_name = 'Perfil_Correo_Electronico',
        @recipients = nombre_cliente,
        @subject = 'Factura de Venta',
        @body = @factura,
        @attach_query = 'SELECT * FROM ventas_detalles WHERE id_venta = @id_venta',
        @attach_file_name = 'Detalles_Venta.csv',
        @attach_content_type = 'text/csv';
END;
```