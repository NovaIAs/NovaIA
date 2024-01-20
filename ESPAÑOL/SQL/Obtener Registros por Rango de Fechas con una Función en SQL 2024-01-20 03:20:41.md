```sql
-- Este código crea una función que devuelve todos los registros de una tabla filtrados por un rango de fechas dado.

CREATE FUNCTION [dbo].[ObtenerRegistrosPorRangoDeFechas]
(
    @Tabla VARCHAR(128), -- El nombre de la tabla de la que se quieren obtener los registros.
    @FechaInicio DATE, -- La fecha de inicio del rango de fechas.
    @FechaFin DATE -- La fecha de fin del rango de fechas.
)
RETURNS TABLE
AS
BEGIN
    -- Se declara una variable para almacenar la consulta SQL.
    DECLARE @Consulta NVARCHAR(MAX);

    -- Se construye la consulta SQL.
    SET @Consulta = 'SELECT * FROM ' + @Tabla + ' WHERE Fecha BETWEEN @FechaInicio AND @FechaFin';

    -- Se ejecuta la consulta SQL y se devuelven los resultados.
    RETURN (
        EXECUTE sp_executesql
        (
            @Consulta,
            N'@FechaInicio DATE, @FechaFin DATE',
            @FechaInicio,
            @FechaFin
        )
    );
END;

-- Este código muestra cómo utilizar la función creada anteriormente.

SELECT * FROM [dbo].[ObtenerRegistrosPorRangoDeFechas]('Tabla1', '2023-01-01', '2023-12-31');
```

**Explicación del código:**

* La función `ObtenerRegistrosPorRangoDeFechas` se crea utilizando la instrucción `CREATE FUNCTION`.
* La función recibe tres parámetros: el nombre de la tabla de la que se quieren obtener los registros, la fecha de inicio del rango de fechas y la fecha de fin del rango de fechas.
* La función devuelve una tabla con los registros de la tabla especificada que se encuentran dentro del rango de fechas dado.
* La función se utiliza en la instrucción `SELECT` para obtener los registros de la tabla `Tabla1` que se encuentran entre el 1 de enero de 2023 y el 31 de diciembre de 2023.