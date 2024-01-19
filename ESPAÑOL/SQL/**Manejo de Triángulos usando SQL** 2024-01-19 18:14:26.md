```sql
-- Crear una función que calcula el área de un triángulo.
CREATE FUNCTION area_triangulo(base NUMERIC, altura NUMERIC)
RETURNS NUMERIC
AS
$$
BEGIN
  -- Comprobar si los parámetros son válidos.
  IF base <= 0 OR altura <= 0 THEN
    RAISE ERROR 'Los parámetros deben ser positivos.'
  END IF;

  -- Calcular el área del triángulo.
  RETURN 0.5 * base * altura;
END;
$$
LANGUAGE plpgsql;

-- Crear una tabla para almacenar los triángulos.
CREATE TABLE triangulos (
  id SERIAL PRIMARY KEY,
  base NUMERIC NOT NULL,
  altura NUMERIC NOT NULL,
  area NUMERIC NOT NULL
);

-- Insertar algunos triángulos en la tabla.
INSERT INTO triangulos (base, altura, area) VALUES
  (3, 4, area_triangulo(3, 4)),
  (5, 6, area_triangulo(5, 6)),
  (7, 8, area_triangulo(7, 8));

-- Seleccionar todos los triángulos de la tabla.
SELECT * FROM triangulos;

-- Seleccionar los triángulos con un área mayor que 10.
SELECT * FROM triangulos WHERE area > 10;

-- Eliminar todos los triángulos de la tabla.
DELETE FROM triangulos;

-- Eliminar la tabla triángulos.
DROP TABLE triangulos;

-- Eliminar la función area_triangulo.
DROP FUNCTION area_triangulo;
```

Explicación:

1. Creamos una función llamada `area_triangulo` que calcula el área de un triángulo dados su base y altura. La función está escrita en el lenguaje de programación PL/pgSQL.
2. Creamos una tabla llamada `triangulos` para almacenar los triángulos. La tabla tiene tres columnas: `id`, `base`, `altura` y `area`.
3. Insertamos algunos triángulos en la tabla usando la función `area_triangulo` para calcular el área de cada triángulo.
4. Seleccionamos todos los triángulos de la tabla.
5. Seleccionamos los triángulos con un área mayor que 10.
6. Eliminamos todos los triángulos de la tabla.
7. Eliminamos la tabla `triángulos`.
8. Eliminamos la función `area_triangulo`.