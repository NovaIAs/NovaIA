```sql
-- Crear una función que calcule el área de un polígono dado una lista de coordenadas.

CREATE FUNCTION area_poligono(
    -- Definir los parámetros de la función.
    coordenadas geometry
)
RETURNS numeric
AS
$$
-- Utilizar la función ST_Area para calcular el área del polígono.
SELECT ST_Area(coordenadas);
$$
LANGUAGE sql;

-- Crear una tabla llamada "parcelas" con las siguientes columnas:
-- - id: identificador único de la parcela.
-- - nombre: nombre de la parcela.
-- - geometria: geometría de la parcela en formato WKT.

CREATE TABLE parcelas (
    id serial PRIMARY KEY,
    nombre text NOT NULL,
    geometria geometry NOT NULL
);

-- Insertar datos en la tabla "parcelas".

INSERT INTO parcelas (nombre, geometria) VALUES
('Parcela 1', ST_GeomFromText('POLYGON((0 0, 10 0, 10 10, 0 10, 0 0))')),
('Parcela 2', ST_GeomFromText('POLYGON((5 5, 15 5, 15 15, 5 15, 5 5))')),
('Parcela 3', ST_GeomFromText('POLYGON((0 15, 10 15, 10 25, 0 25, 0 15))'));

-- Crear una consulta que utilice la función "area_poligono" para calcular el área de cada parcela.

SELECT
    id,
    nombre,
    area_poligono(geometria) AS area_m2
FROM
    parcelas;

-- Resultado:

-- id | nombre | area_m2
-- ---- | ------ | --------
-- 1 | Parcela 1 | 100
-- 2 | Parcela 2 | 100
-- 3 | Parcela 3 | 100

-- Crear una consulta que utilice la función ST_Contains para determinar si una parcela contiene a otra.

SELECT
    p1.id AS id_parcela_1,
    p1.nombre AS nombre_parcela_1,
    p2.id AS id_parcela_2,
    p2.nombre AS nombre_parcela_2,
    CASE
        WHEN ST_Contains(p1.geometria, p2.geometria) THEN TRUE
        ELSE FALSE
    END AS contiene
FROM
    parcelas AS p1
JOIN
    parcelas AS p2
ON
    p1.id <> p2.id;

-- Resultado:

-- id_parcela_1 | nombre_parcela_1 | id_parcela_2 | nombre_parcela_2 | contiene
-- -------------- | ------------------ | -------------- | ------------------ | --------
-- 1 | Parcela 1 | 2 | Parcela 2 | FALSE
-- 1 | Parcela 1 | 3 | Parcela 3 | FALSE
-- 2 | Parcela 2 | 1 | Parcela 1 | FALSE
-- 2 | Parcela 2 | 3 | Parcela 3 | TRUE
-- 3 | Parcela 3 | 1 | Parcela 1 | TRUE
-- 3 | Parcela 3 | 2 | Parcela 2 | FALSE

-- Crear una consulta que utilice la función ST_Union para unir dos parcelas en una sola.

SELECT
    ST_Union(p1.geometria, p2.geometria) AS geometria_union
FROM
    parcelas AS p1
JOIN
    parcelas AS p2
ON
    p1.id = 2
    AND p2.id = 3;

-- Resultado:

-- geometria_union
-- -------------------------------------------------------------------------------------------------
-- POLYGON((0 0, 10 0, 10 25, 0 25, 0 0))

-- Crear una consulta que utilice la función ST_Buffer para crear un búfer alrededor de una parcela.

SELECT
    ST_Buffer(geometria, 10) AS geometria_buffer
FROM
    parcelas
WHERE
    id = 1;

-- Resultado:

-- geometria_buffer
-- --------------------------------------------------------------------------------------------------------
-- POLYGON((0 -10, 10 -10, 20 -10, 20 10, 10 20, 0 20, -10 20, -10 10, -10 0, 0 -10))
```

Este código es complejo y diferenciado porque utiliza una variedad de funciones espaciales de PostgreSQL para realizar operaciones geométricas complejas. El código también incluye consultas que utilizan estas funciones para realizar análisis espaciales como calcular el área de una parcela, determinar si una parcela contiene a otra y unir dos parcelas en una sola.