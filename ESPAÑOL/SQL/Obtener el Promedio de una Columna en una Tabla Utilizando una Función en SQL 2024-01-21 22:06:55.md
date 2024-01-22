```sql
-- Crear una función que calcule el promedio de una columna en una tabla.

CREATE FUNCTION obtener_promedio(@tabla VARCHAR(255), @columna VARCHAR(255))
RETURNS FLOAT
AS
BEGIN
    -- Comprobar si la tabla existe.

    IF NOT EXISTS (SELECT * FROM INFORMATION_SCHEMA.TABLES WHERE TABLE_NAME = @tabla)
    BEGIN
        RAISERROR('La tabla "%s" no existe.', 16, 1, @tabla);
        RETURN NULL;
    END;

    -- Comprobar si la columna existe.

    IF NOT EXISTS (SELECT * FROM INFORMATION_SCHEMA.COLUMNS WHERE TABLE_NAME = @tabla AND COLUMN_NAME = @columna)
    BEGIN
        RAISERROR('La columna "%s" no existe en la tabla "%s".', 16, 1, @columna, @tabla);
        RETURN NULL;
    END;

    -- Calcular el promedio de la columna.

    DECLARE @promedio FLOAT;

    SELECT @promedio = AVG(@columna) FROM @tabla;

    -- Devolver el promedio.

    RETURN @promedio;
END;

-- Utilizar la función para calcular el promedio de la columna "Precio" en la tabla "Productos".

DECLARE @promedio_precio FLOAT;

SELECT @promedio_precio = obtener_promedio('Productos', 'Precio');

-- Imprimir el promedio en la consola.

SELECT 'El promedio de precio de los productos es:', @promedio_precio;
```

Explicación:

* La función `obtener_promedio()` calcula el promedio de una columna en una tabla.

* La función toma dos parámetros:

    * `@tabla`: El nombre de la tabla que contiene la columna.
    * `@columna`: El nombre de la columna cuyo promedio se desea calcular.

* La función comienza comprobando si la tabla existe. Si la tabla no existe, se devuelve un error.

* A continuación, la función comprueba si la columna existe. Si la columna no existe, se devuelve un error.

* Si la tabla y la columna existen, la función calcula el promedio de la columna utilizando la función `AVG()`.

* El promedio se devuelve como un valor de retorno de la función.

* La función `obtener_promedio()` se utiliza para calcular el promedio de la columna "Precio" en la tabla "Productos".

* El promedio se imprime en la consola.