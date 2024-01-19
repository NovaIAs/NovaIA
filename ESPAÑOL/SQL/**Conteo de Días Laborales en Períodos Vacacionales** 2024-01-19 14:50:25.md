```sql
-- Crear una función que devuelva el número de días laborables entre dos fechas

CREATE FUNCTION DiasLaborables(@FechaInicio DATE, @FechaFin DATE)
RETURNS INT
AS
BEGIN
    -- Calcular el número de días entre las dos fechas
    DECLARE @DiasTotales INT = DATEDIFF(DAY, @FechaInicio, @FechaFin) + 1;

    -- Calcular el número de fines de semana entre las dos fechas
    DECLARE @DiasFindeSemana INT = 0;
    WHILE @FechaInicio <= @FechaFin
    BEGIN
        IF DATEPART(DW, @FechaInicio) IN (7, 1) -- Sábado o domingo
        BEGIN
            @DiasFindeSemana += 1;
        END;
        SET @FechaInicio = DATEADD(DAY, 1, @FechaInicio);
    END;

    -- Restar el número de fines de semana del número total de días
    RETURN @DiasTotales - @DiasFindeSemana;
END;

-- Crear una tabla con datos de empleados y sus fechas de inicio y fin de vacaciones

CREATE TABLE EmpleadosVacaciones (
    EmpleadoID INT NOT NULL,
    NombreEmpleado VARCHAR(50) NOT NULL,
    FechaInicioVacaciones DATE NOT NULL,
    FechaFinVacaciones DATE NOT NULL,
    CONSTRAINT PK_EmpleadosVacaciones PRIMARY KEY (EmpleadoID)
);

-- Insertar datos en la tabla EmpleadosVacaciones

INSERT INTO EmpleadosVacaciones (EmpleadoID, NombreEmpleado, FechaInicioVacaciones, FechaFinVacaciones)
VALUES
(1, 'Juan Pérez', '2023-07-01', '2023-07-10'),
(2, 'María López', '2023-08-15', '2023-08-21'),
(3, 'Pedro García', '2023-09-01', '2023-09-15'),
(4, 'Ana Rodríguez', '2023-10-01', '2023-10-10'),
(5, 'José González', '2023-11-01', '2023-11-15');

-- Crear una vista que muestre el nombre del empleado, las fechas de inicio y fin de sus vacaciones, y el número de días laborables de vacaciones

CREATE VIEW EmpleadosVacacionesVista AS
SELECT
    e.NombreEmpleado,
    e.FechaInicioVacaciones,
    e.FechaFinVacaciones,
    DiasLaborables(e.FechaInicioVacaciones, e.FechaFinVacaciones) AS DiasLaborablesVacaciones
FROM
    EmpleadosVacaciones e;

-- Seleccionar los datos de la vista EmpleadosVacacionesVista

SELECT * FROM EmpleadosVacacionesVista;
```

Explicación del código:

* Se crea una función llamada `DiasLaborables` que devuelve el número de días laborables entre dos fechas. La función utiliza la función `DATEDIFF` para calcular el número total de días entre las dos fechas y luego resta el número de fines de semana (sábados y domingos) del número total de días.
* Se crea una tabla llamada `EmpleadosVacaciones` que almacena datos de empleados y sus fechas de inicio y fin de vacaciones.
* Se insertan datos en la tabla `EmpleadosVacaciones`.
* Se crea una vista llamada `EmpleadosVacacionesVista` que muestra el nombre del empleado, las fechas de inicio y fin de sus vacaciones, y el número de días laborables de vacaciones. La vista utiliza la función `DiasLaborables` para calcular el número de días laborables de vacaciones.
* Se seleccionan los datos de la vista `EmpleadosVacacionesVista`.