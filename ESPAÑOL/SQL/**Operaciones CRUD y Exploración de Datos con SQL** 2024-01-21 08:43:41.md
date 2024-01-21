```sql
-- Crear una función que calcule el promedio de una lista de números
CREATE FUNCTION promedio(numeros LIST<INT64>) RETURNS FLOAT64
AS (
  -- Comprobar si la lista está vacía
  IF EMPTY(numeros) THEN
    -- Devolver 0 si la lista está vacía
    RETURN 0.0;
  ELSE
    -- Calcular la suma de los números en la lista
    suma = 0;
    FOR num IN numeros DO
      suma += num;
    END FOR;

    -- Calcular el promedio de los números en la lista
    promedio = suma / ARRAY_LENGTH(numeros);

    -- Devolver el promedio
    RETURN promedio;
  END IF
);

-- Crear una tabla con los datos de los estudiantes
CREATE TABLE estudiantes (
  id INT64 NOT NULL AUTO_INCREMENT,
  nombre STRING(255) NOT NULL,
  calificaciones LIST<INT64> NOT NULL,
  promedio FLOAT64 NOT NULL,
  PRIMARY KEY (id)
);

-- Insertar datos en la tabla de estudiantes
INSERT INTO estudiantes (nombre, calificaciones, promedio) VALUES
  ('Juan', [90, 85, 95], promedio([90, 85, 95])),
  ('María', [80, 90, 85], promedio([80, 90, 85])),
  ('Pedro', [75, 80, 90], promedio([75, 80, 90])),
  ('Ana', [95, 90, 85], promedio([95, 90, 85]));

-- Seleccionar los datos de los estudiantes con un promedio mayor a 85
SELECT * FROM estudiantes
WHERE promedio > 85;

-- Actualizar el promedio de los estudiantes
UPDATE estudiantes
SET promedio = promedio([90, 85, 95])
WHERE nombre = 'Juan';

-- Eliminar los estudiantes con un promedio menor a 80
DELETE FROM estudiantes
WHERE promedio < 80;

-- Crear una vista que muestre el nombre y el promedio de los estudiantes
CREATE VIEW vista_estudiantes AS
SELECT nombre, promedio FROM estudiantes;

-- Seleccionar los datos de la vista de estudiantes
SELECT * FROM vista_estudiantes;

-- Crear un índice en la columna nombre de la tabla de estudiantes
CREATE INDEX idx_nombre ON estudiantes (nombre);

-- Analizar la tabla de estudiantes
ANALYZE TABLE estudiantes;

-- Mostrar el plan de ejecución de una consulta
EXPLAIN SELECT * FROM estudiantes;
```

Este código realiza las siguientes operaciones:

1. Crea una función llamada `promedio` que calcula el promedio de una lista de números.
2. Crea una tabla llamada `estudiantes` con las columnas `id`, `nombre`, `calificaciones` y `promedio`.
3. Inserta datos en la tabla de estudiantes.
4. Selecciona los datos de los estudiantes con un promedio mayor a 85.
5. Actualiza el promedio de los estudiantes con el nombre "Juan".
6. Elimina los estudiantes con un promedio menor a 80.
7. Crea una vista llamada `vista_estudiantes` que muestra el nombre y el promedio de los estudiantes.
8. Selecciona los datos de la vista de estudiantes.
9. Crea un índice en la columna `nombre` de la tabla de estudiantes.
10. Analiza la tabla de estudiantes.
11. Muestra el plan de ejecución de la consulta `SELECT * FROM estudiantes`.