```sql

-- Crear una tabla temporal para almacenar los datos intermedios.
WITH TablaTemporal AS (
    SELECT
        ID,
        Nombre,
        Apellido,
        Ciudad,
        CalificaciónPromedio
    FROM Estudiantes
    WHERE CalificaciónPromedio > 80
)

-- Seleccionar los datos de la tabla temporal y calcular la calificación máxima.
SELECT
    ID,
    Nombre,
    Apellido,
    Ciudad,
    CalificaciónPromedio,
    MAX(CalificaciónPromedio) OVER (PARTITION BY Ciudad) AS CalificaciónMáximaEnCiudad
FROM TablaTemporal

-- Ordenar los resultados por ciudad y calificación promedio.
ORDER BY Ciudad, CalificaciónPromedio DESC;
```

Explicación del código:



* La primera consulta crea una tabla temporal llamada `TablaTemporal` que contiene los datos de los estudiantes con una calificación promedio superior a 80.


* La segunda consulta selecciona los datos de la tabla temporal y calcula la calificación máxima para cada ciudad utilizando la función `MAX()` con la cláusula `PARTITION BY`.


* Por último, la consulta ordena los resultados por ciudad y calificación promedio en orden descendente.


Este código es complejo porque utiliza una tabla temporal, la función `MAX()` con la cláusula `PARTITION BY` y la cláusula `ORDER BY`. También es difícil de repetir porque es específico para el conjunto de datos y el problema que se está resolviendo.