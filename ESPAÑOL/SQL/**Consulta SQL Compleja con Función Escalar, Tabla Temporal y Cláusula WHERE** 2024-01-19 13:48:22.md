```sql
-- Crear una función escalar que devuelva el número de días entre dos fechas.

CREATE FUNCTION dbo.DiasEntreFechas(@Fecha1 DATETIME, @Fecha2 DATETIME)
RETURNS INT
AS
BEGIN
    -- Calcular la diferencia entre las dos fechas en días.

    DECLARE @Dias INT;

    SET @Dias = DATEDIFF(DD, @Fecha1, @Fecha2);

    -- Devolver el resultado.

    RETURN @Dias;
END;

-- Crear una tabla temporal con los datos de los empleados.

CREATE TEMP TABLE #Empleados (
    IDEmpleado INT PRIMARY KEY,
    Nombre VARCHAR(50),
    FechaNacimiento DATETIME,
    FechaContratacion DATETIME
);

-- Insertar datos en la tabla temporal.

INSERT INTO #Empleados (IDEmpleado, Nombre, FechaNacimiento, FechaContratacion) VALUES
(1, 'Juan Pérez', '1980-01-01', '2010-01-01'),
(2, 'María Martínez', '1985-07-15', '2012-02-15'),
(3, 'Pedro González', '1990-04-20', '2015-03-20'),
(4, 'Ana Sánchez', '1995-11-25', '2018-04-25'),
(5, 'Luis Rodríguez', '2000-06-30', '2021-05-30');

-- Seleccionar los datos de los empleados que llevan más de 5 años en la empresa.

SELECT IDEmpleado, Nombre, FechaNacimiento, FechaContratacion
FROM #Empleados
WHERE DiasEntreFechas(FechaContratacion, GETDATE()) > 1825;

-- Eliminar la tabla temporal.

DROP TABLE #Empleados;
```

Este código es un ejemplo de una consulta SQL compleja que utiliza una función escalar, una tabla temporal y una cláusula WHERE con una expresión de comparación.

La función escalar `DiasEntreFechas` calcula el número de días entre dos fechas dadas. Esta función se utiliza en la cláusula WHERE para filtrar los datos de la tabla temporal `#Empleados`.

La tabla temporal `#Empleados` se crea utilizando la instrucción `CREATE TEMP TABLE`. Esta tabla contiene los datos de los empleados, incluyendo su ID, nombre, fecha de nacimiento y fecha de contratación.

La instrucción `INSERT INTO` se utiliza para insertar datos en la tabla temporal. Los datos se insertan en la tabla en el orden en que aparecen en la instrucción `VALUES`.

La instrucción `SELECT` se utiliza para seleccionar los datos de la tabla temporal que cumplen con la condición especificada en la cláusula WHERE. En este caso, la cláusula WHERE filtra los datos de los empleados que llevan más de 5 años en la empresa.

La expresión de comparación `DiasEntreFechas(FechaContratacion, GETDATE()) > 1825` calcula el número de días entre la fecha de contratación del empleado y la fecha actual. Si el número de días es mayor que 1825 (5 años), el empleado se incluye en los resultados de la consulta.

La instrucción `DROP TABLE` se utiliza para eliminar la tabla temporal `#Empleados`.