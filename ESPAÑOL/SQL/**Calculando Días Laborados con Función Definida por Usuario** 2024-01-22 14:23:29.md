```sql
-- Crear una función escalar definida por el usuario (UDF) que devuelva el número de días entre dos fechas.

CREATE FUNCTION [dbo].[DiasEntreFechas] (
    @FechaInicio DATE,
    @FechaFin DATE
)
RETURNS INT
AS
BEGIN
    -- Calcular la diferencia entre las dos fechas en días.

    DECLARE @Dias INT;

    SET @Dias = DATEDIFF(DAY, @FechaInicio, @FechaFin);

    -- Devolver el número de días.

    RETURN @Dias;
END;

-- Crear una tabla temporal para almacenar los datos de los empleados y sus fechas de inicio y fin de empleo.

CREATE TABLE #Empleados (
    EmpleadoID INT,
    Nombre VARCHAR(50),
    FechaInicio DATE,
    FechaFin DATE
);

-- Insertar datos en la tabla temporal.

INSERT INTO #Empleados (EmpleadoID, Nombre, FechaInicio, FechaFin)
VALUES
(1, 'Juan Pérez', '2020-01-01', '2022-12-31'),
(2, 'María García', '2019-07-01', '2021-06-30'),
(3, 'Pedro López', '2018-04-01', '2022-03-31'),
(4, 'Ana Sánchez', '2021-01-01', '2023-12-31'),
(5, 'José Rodríguez', '2020-07-01', '2022-09-30');

-- Crear una consulta para seleccionar el nombre del empleado, la fecha de inicio y la fecha de fin de empleo, y el número de días trabajados.

SELECT
    Nombre,
    FechaInicio,
    FechaFin,
    [dbo].[DiasEntreFechas](FechaInicio, FechaFin) AS DiasTrabajados
FROM
    #Empleados;

-- Eliminar la tabla temporal.

DROP TABLE #Empleados;
```

Explicación:

* La función escalar definida por el usuario (UDF) `[dbo].[DiasEntreFechas]` se utiliza para calcular el número de días entre dos fechas. La función toma dos parámetros de entrada, `@FechaInicio` y `@FechaFin`, y devuelve un valor entero que representa el número de días entre las dos fechas.
* La tabla temporal `#Empleados` se utiliza para almacenar los datos de los empleados y sus fechas de inicio y fin de empleo. La tabla se crea utilizando la instrucción `CREATE TABLE` y los datos se insertan en la tabla utilizando la instrucción `INSERT`.
* La consulta `SELECT` se utiliza para seleccionar el nombre del empleado, la fecha de inicio, la fecha de fin y el número de días trabajados. La función `[dbo].[DiasEntreFechas]` se utiliza para calcular el número de días trabajados.
* La instrucción `DROP TABLE` se utiliza para eliminar la tabla temporal `#Empleados`.