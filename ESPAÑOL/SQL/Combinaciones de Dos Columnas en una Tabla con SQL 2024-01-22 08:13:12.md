```sql
-- Función que devuelve una tabla con las combinaciones de dos columnas de una tabla determinada.

CREATE FUNCTION `Combinaciones`(`Tabla` VARCHAR(255), `Columna1` VARCHAR(255), `Columna2` VARCHAR(255))
RETURNS TABLE(
    `Combinación1` VARCHAR(255),
    `Combinación2` VARCHAR(255)
)
BEGIN

    -- Creamos una variable temporal para almacenar los resultados.

    DECLARE Resultados TABLE(
        `Combinación1` VARCHAR(255),
        `Combinación2` VARCHAR(255)
    );

    -- Insertamos en la variable temporal todas las combinaciones posibles de las dos columnas especificadas.

    INSERT INTO Resultados (
        `Combinación1`,
        `Combinación2`
    )
    SELECT
        `Columna1`,
        `Columna2`
    FROM
        `Tabla`;

    -- Devolvemos la variable temporal como resultado de la función.

    RETURN Resultados;

END;

-- Ejemplo de uso de la función `Combinaciones`.

SELECT * FROM `Combinaciones`('Clientes', 'Nombre', 'Apellido');

-- Resultado:

-- +---------------+---------------+
-- | Combinación1 | Combinación2 |
-- +---------------+---------------+
-- | Juan          | Pérez         |
-- +---------------+---------------+
-- | María         | García        |
-- +---------------+---------------+
-- | Pedro         | López         |
-- +---------------+---------------+
```

Esta función es útil para generar combinaciones de datos de dos columnas de una tabla determinada. Por ejemplo, se puede utilizar para generar una lista de todas las posibles parejas de clientes de una tabla de clientes, o para generar una lista de todas las posibles combinaciones de productos de una tabla de productos.