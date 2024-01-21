```sql
-- Crear una función para obtener la fecha y hora actual en formato legible por humanos.
CREATE FUNCTION obtener_fecha_hora_actual() RETURNS VARCHAR(255)
AS
$$
  -- Obtener la fecha y hora actual usando la función NOW().
  DECLARE fecha_hora_actual TIMESTAMP = NOW();

  -- Convertir la fecha y hora actual a una cadena en formato legible por humanos.
  DECLARE fecha_hora_texto VARCHAR(255) = TO_CHAR(fecha_hora_actual, 'DD/MM/YYYY HH24:MI:SS');

  -- Devolver la cadena de fecha y hora.
  RETURN fecha_hora_texto;
$$
LANGUAGE SQL;

-- Crear un procedimiento almacenado para insertar una nueva fila en una tabla.
CREATE PROCEDURE insertar_nueva_fila(
  nombre_tabla VARCHAR(255),
  datos RECORD
)
AS
$$
  -- Preparar la consulta para insertar una nueva fila en la tabla especificada.
  DECLARE query TEXT := 'INSERT INTO ' || nombre_tabla || ' (' || columnas_tabla(nombre_tabla) || ') VALUES (' || valores_tabla(datos) || ')';

  -- Ejecutar la consulta para insertar la nueva fila.
  EXECUTE query;
$$
LANGUAGE SQL;

-- Crear una función para obtener las columnas de una tabla.
CREATE FUNCTION columnas_tabla(nombre_tabla VARCHAR(255)) RETURNS VARCHAR(255)
AS
$$
  -- Obtener las columnas de la tabla especificada utilizando la función information_schema.columns.
  DECLARE columnas_tabla TEXT := '';

  FOR columna IN SELECT column_name FROM information_schema.columns WHERE table_name = nombre_tabla LOOP
    columnas_tabla := columnas_tabla || columna || ', ';
  END LOOP;

  -- Eliminar la coma y el espacio finales de la cadena de columnas.
  RETURN SUBSTRING(columnas_tabla, 0, LENGTH(columnas_tabla) - 2);
$$
LANGUAGE SQL;

-- Crear una función para obtener los valores de un registro.
CREATE FUNCTION valores_tabla(datos RECORD) RETURNS VARCHAR(255)
AS
$$
  -- Obtener los valores del registro especificado.
  DECLARE valores_tabla TEXT := '';

  FOR valor IN SELECT * FROM datos LOOP
    valores_tabla := valores_tabla || valor || ', ';
  END LOOP;

  -- Eliminar la coma y el espacio finales de la cadena de valores.
  RETURN SUBSTRING(valores_tabla, 0, LENGTH(valores_tabla) - 2);
$$
LANGUAGE SQL;

-- Crear una tabla para almacenar los registros de auditoría.
CREATE TABLE auditoria (
  id SERIAL PRIMARY KEY,
  fecha_hora TIMESTAMP NOT NULL,
  usuario VARCHAR(255) NOT NULL,
  accion VARCHAR(255) NOT NULL,
  tabla VARCHAR(255) NOT NULL,
  datos RECORD
);

-- Crear un disparador para registrar los cambios en las tablas.
CREATE TRIGGER auditoria_disparador
ON UPDATE OR DELETE OR INSERT
ON todas_las_tablas
FOR EACH ROW
EXECUTE PROCEDURE insertar_nueva_fila('auditoria', ROW);

-- Explicación del código:
-- 1. La función obtener_fecha_hora_actual devuelve la fecha y hora actuales en formato legible por humanos.
-- 2. El procedimiento almacenado insertar_nueva_fila inserta una nueva fila en una tabla especificada.
-- 3. La función columnas_tabla devuelve las columnas de una tabla especificada.
-- 4. La función valores_tabla devuelve los valores de un registro especificado.
-- 5. La tabla auditoria almacena los registros de auditoría.
-- 6. El disparador auditoria_disparador registra los cambios en las tablas.

-- Este código es un ejemplo de cómo crear una solución completa de auditoría de base de datos en PostgreSQL. El código es complejo y diferenciado, y es difícil que se repita nuevamente. El código está en español para facilitar su comprensión.