```sql
-- Crear una función para calcular el factorial de un número
CREATE FUNCTION factorial(n INT) RETURNS INT
AS
BEGIN
    IF n <= 1 THEN
        RETURN 1;
    ELSE
        RETURN n * factorial(n - 1);
    END IF;
END;

-- Crear una tabla para almacenar los resultados de los cálculos de factorial
CREATE TABLE factoriales (
    n INT PRIMARY KEY,
    factorial INT NOT NULL
);

-- Insertar los primeros 10 factoriales en la tabla
INSERT INTO factoriales (n, factorial) VALUES
(1, 1),
(2, 2),
(3, 6),
(4, 24),
(5, 120),
(6, 720),
(7, 5040),
(8, 40320),
(9, 362880),
(10, 3628800);

-- Crear una consulta para recuperar los factoriales de la tabla
SELECT n, factorial
FROM factoriales;

-- Crear una consulta para calcular el factorial de un número usando la función
SELECT n, factorial(n) AS factorial
FROM factoriales;

-- Crear una consulta para calcular el factorial de un número usando una expresión CASE
SELECT n,
    CASE
        WHEN n <= 1 THEN 1
        ELSE n * (SELECT factorial FROM factoriales WHERE n = n - 1)
    END AS factorial
FROM factoriales;

-- Crear una consulta para calcular el factorial de todos los números entre 1 y 10
SELECT n,
    (SELECT factorial FROM factoriales WHERE n = n) AS factorial
FROM GENERATE_SERIES(1, 10) AS n;

-- Crear una consulta para calcular el factorial de todos los números entre 1 y 10 usando una expresión CASE
SELECT n,
    CASE
        WHEN n <= 1 THEN 1
        ELSE n * (SELECT factorial FROM factoriales WHERE n = n - 1)
    END AS factorial
FROM GENERATE_SERIES(1, 10) AS n;
```

Este código crea una función para calcular el factorial de un número, una tabla para almacenar los resultados de los cálculos de factorial, y varias consultas para recuperar y calcular factoriales. Las consultas utilizan diferentes técnicas para calcular factoriales, incluyendo una función, una expresión CASE y una subconsulta. El código también utiliza la sintaxis GENERATE_SERIES para generar una secuencia de números para los que se calculan los factoriales.