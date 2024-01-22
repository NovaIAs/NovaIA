```sql
-- Variables de sesión temporales utilizadas en el script
SET @aux_usuario := NULL;
SET @aux_fecha := NULL;
SET @aux_num := 0;

-- Definición de la función escalar 'obtener_diferencia_fechas'
CREATE FUNCTION obtener_diferencia_fechas(fecha1 DATE, fecha2 DATE)
RETURNS INT
DETERMINISTIC
BEGIN
  DECLARE diferencia INT;
  SET diferencia := DATEDIFF(fecha2, fecha1);
  RETURN diferencia;
END;

-- Creación de la tabla temporal 'tabla_temporal'
CREATE TEMPORARY TABLE tabla_temporal (
  id INT NOT NULL,
  usuario VARCHAR(255) NOT NULL,
  fecha DATE NOT NULL,
  valor INT NOT NULL,
  PRIMARY KEY (id)
);

-- Carga de datos en la tabla temporal 'tabla_temporal'
INSERT INTO tabla_temporal (id, usuario, fecha, valor) VALUES
(1, 'usuario_1', '2023-01-01', 100),
(2, 'usuario_2', '2023-01-02', 200),
(3, 'usuario_3', '2023-01-03', 300),
(4, 'usuario_1', '2023-01-04', 400),
(5, 'usuario_5', '2023-01-05', 500),
(6, 'usuario_2', '2023-01-06', 600),
(7, 'usuario_1', '2023-01-07', 700),
(8, 'usuario_3', '2023-01-08', 800),
(9, 'usuario_1', '2023-01-09', 900),
(10, 'usuario_4', '2023-01-10', 1000);

-- Obtener el nombre de usuario, su última fecha de registro y la cantidad de días
-- transcurridos desde su última fecha de registro hasta la fecha actual
SELECT
  t.usuario,
  MAX(t.fecha) AS ultima_fecha,
  obtener_diferencia_fechas(MAX(t.fecha), CURRENT_DATE()) AS dias_transcurridos
FROM tabla_temporal t
GROUP BY t.usuario
ORDER BY ultima_fecha DESC;

-- Incorporar en el resultado anterior una columna llamada 'retraso_promedio' que
-- indique el retraso promedio en días de los registros del usuario con respecto
-- a la fecha actual
SELECT
  t.usuario,
  MAX(t.fecha) AS ultima_fecha,
  obtener_diferencia_fechas(MAX(t.fecha), CURRENT_DATE()) AS dias_transcurridos,
  AVG(obtener_diferencia_fechas(t.fecha, CURRENT_DATE())) AS retraso_promedio
FROM tabla_temporal t
GROUP BY t.usuario
ORDER BY ultima_fecha DESC;

-- Incorporar en el resultado anterior una columna llamada 'retraso_maximo' que
-- indique el retraso máximo en días de los registros del usuario con respecto
-- a la fecha actual
SELECT
  t.usuario,
  MAX(t.fecha) AS ultima_fecha,
  obtener_diferencia_fechas(MAX(t.fecha), CURRENT_DATE()) AS dias_transcurridos,
  AVG(obtener_diferencia_fechas(t.fecha, CURRENT_DATE())) AS retraso_promedio,
  MAX(obtener_diferencia_fechas(t.fecha, CURRENT_DATE())) AS retraso_maximo
FROM tabla_temporal t
GROUP BY t.usuario
ORDER BY ultima_fecha DESC;

-- Incorporar en el resultado anterior una columna llamada 'retraso_minimo' que
-- indique el retraso mínimo en días de los registros del usuario con respecto
-- a la fecha actual
SELECT
  t.usuario,
  MAX(t.fecha) AS ultima_fecha,
  obtener_diferencia_fechas(MAX(t.fecha), CURRENT_DATE()) AS dias_transcurridos,
  AVG(obtener_diferencia_fechas(t.fecha, CURRENT_DATE())) AS retraso_promedio,
  MAX(obtener_diferencia_fechas(t.fecha, CURRENT_DATE())) AS retraso_maximo,
  MIN(obtener_diferencia_fechas(t.fecha, CURRENT_DATE())) AS retraso_minimo
FROM tabla_temporal t
GROUP BY t.usuario
ORDER BY ultima_fecha DESC;
```

Explicación:

- Se definen las variables de sesión temporales `@aux_usuario`, `@aux_fecha` y `@aux_num` para utilizarlas en el script.

- Se crea la función escalar `obtener_diferencia_fechas` que devuelve la diferencia entre dos fechas en días.

- Se crea la tabla temporal `tabla_temporal` para almacenar los datos que se analizarán en el script.

- Se cargan los datos en la tabla temporal `tabla_temporal` utilizando la instrucción `INSERT`.

- Se ejecuta una consulta para obtener el nombre de usuario, su última fecha de registro y la cantidad de días transcurridos desde su última fecha de registro hasta la fecha actual.

- Se ejecuta una consulta para incorporar una columna llamada `retraso_promedio` en el resultado anterior, que indique el retraso promedio en días de los registros del usuario con respecto a la fecha actual.

- Se ejecuta una consulta para incorporar una columna llamada `retraso_maximo` en el resultado anterior, que indique el retraso máximo en días de los registros del usuario con respecto a la fecha actual.

- Se ejecuta una consulta para incorporar una columna llamada `retraso_minimo` en el resultado anterior, que indique el retraso mínimo en días de los registros del usuario con respecto a la fecha actual.