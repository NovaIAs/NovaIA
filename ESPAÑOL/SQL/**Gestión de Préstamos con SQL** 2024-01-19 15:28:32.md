```sql
-- Crear una función para calcular el pago mensual de un préstamo.
CREATE FUNCTION [dbo].[CalcularPagoMensual]
(
    @principal DECIMAL(18, 2), -- Monto principal del préstamo.
    @tasa_anual DECIMAL(5, 2), -- Tasa de interés anual del préstamo.
    @num_anos INT -- Número de años del préstamo.
)
RETURNS DECIMAL(18, 2)
AS
BEGIN
    -- Calcular la tasa de interés mensual.
    DECLARE @tasa_mensual DECIMAL(5, 2) = @tasa_anual / 12;
    
    -- Calcular el número de pagos mensuales.
    DECLARE @num_pagos INT = @num_anos * 12;
    
    -- Calcular el pago mensual.
    DECLARE @pago_mensual DECIMAL(18, 2) = (@principal * @tasa_mensual) / (1 - POWER(1 + @tasa_mensual, -@num_pagos));
    
    -- Devolver el pago mensual.
    RETURN @pago_mensual;
END;

-- Crear una tabla para almacenar los datos de los préstamos.
CREATE TABLE Prestamos
(
    id_prestamo INT IDENTITY(1, 1) PRIMARY KEY,
    principal DECIMAL(18, 2) NOT NULL,
    tasa_anual DECIMAL(5, 2) NOT NULL,
    num_anos INT NOT NULL,
    pago_mensual DECIMAL(18, 2) NOT NULL AS (CalcularPagoMensual(principal, tasa_anual, num_anos)) PERSISTED,
    fecha_creacion DATETIME DEFAULT GETDATE(),
    fecha_modificacion DATETIME DEFAULT GETDATE()
);

-- Crear un procedimiento almacenado para agregar un nuevo préstamo a la tabla.
CREATE PROCEDURE [dbo].[AgregarPrestamo]
(
    @principal DECIMAL(18, 2), -- Monto principal del préstamo.
    @tasa_anual DECIMAL(5, 2), -- Tasa de interés anual del préstamo.
    @num_anos INT -- Número de años del préstamo.
)
AS
BEGIN
    -- Insertar el nuevo préstamo en la tabla.
    INSERT INTO Prestamos (principal, tasa_anual, num_anos)
    VALUES (@principal, @tasa_anual, @num_anos);
END;

-- Crear un procedimiento almacenado para obtener todos los préstamos de la tabla.
CREATE PROCEDURE [dbo].[ObtenerPrestamos]
AS
BEGIN
    -- Seleccionar todos los préstamos de la tabla.
    SELECT * FROM Prestamos;
END;

-- Crear un procedimiento almacenado para obtener un préstamo por su ID.
CREATE PROCEDURE [dbo].[ObtenerPrestamoPorId]
(
    @id_prestamo INT -- ID del préstamo.
)
AS
BEGIN
    -- Seleccionar el préstamo con el ID especificado.
    SELECT * FROM Prestamos WHERE id_prestamo = @id_prestamo;
END;

-- Crear un procedimiento almacenado para actualizar un préstamo.
CREATE PROCEDURE [dbo].[ActualizarPrestamo]
(
    @id_prestamo INT, -- ID del préstamo.
    @principal DECIMAL(18, 2), -- Monto principal del préstamo.
    @tasa_anual DECIMAL(5, 2), -- Tasa de interés anual del préstamo.
    @num_anos INT -- Número de años del préstamo.
)
AS
BEGIN
    -- Actualizar el préstamo con el ID especificado.
    UPDATE Prestamos SET principal = @principal, tasa_anual = @tasa_anual, num_anos = @num_anos, pago_mensual = CalcularPagoMensual(@principal, @tasa_anual, @num_años) WHERE id_prestamo = @id_prestamo;
END;

-- Crear un procedimiento almacenado para eliminar un préstamo.
CREATE PROCEDURE [dbo].[EliminarPrestamo]
(
    @id_prestamo INT -- ID del préstamo.
)
AS
BEGIN
    -- Eliminar el préstamo con el ID especificado.
    DELETE FROM Prestamos WHERE id_prestamo = @id_prestamo;
END;
```

Explicación del código:

* La función `CalcularPagoMensual` calcula el pago mensual de un préstamo dado el monto principal, la tasa de interés anual y el número de años del préstamo.
* La tabla `Prestamos` almacena los datos de los préstamos, incluyendo el monto principal, la tasa de interés anual, el número de años del préstamo y el pago mensual.
* El procedimiento almacenado `AgregarPrestamo` agrega un nuevo préstamo a la tabla `Prestamos`.
* El procedimiento almacenado `ObtenerPrestamos` obtiene todos los préstamos de la tabla `Prestamos`.
* El procedimiento almacenado `ObtenerPrestamoPorId` obtiene un préstamo por su ID de la tabla `Prestamos`.
* El procedimiento almacenado `ActualizarPrestamo` actualiza un préstamo en la tabla `Prestamos`.
* El procedimiento almacenado `EliminarPrestamo` elimina un préstamo de la tabla `Prestamos`.