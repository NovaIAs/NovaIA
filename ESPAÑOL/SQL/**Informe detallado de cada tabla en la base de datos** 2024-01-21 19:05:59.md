```sql
-- Crear una vista temporal para contar el número de registros en cada tabla en la base de datos.

CREATE TEMP TABLE Tabla_Recuento_Registros AS
SELECT
    table_schema,
    table_name,
    COUNT(*) AS registro_cuenta
FROM
    information_schema.tables
WHERE
    table_schema NOT IN ('information_schema', 'pg_catalog') -- excluir tablas del sistema
GROUP BY
    table_schema, table_name;

-- Crear una vista temporal para enumerar todas las columnas y sus tipos de datos en cada tabla.

CREATE TEMP TABLE Tabla_Columnas_Tipos_Datos AS
SELECT
    table_schema,
    table_name,
    column_name,
    data_type
FROM
    information_schema.columns
WHERE
    table_schema NOT IN ('information_schema', 'pg_catalog'); -- excluir tablas del sistema;

-- Crear una vista temporal para listar todas las restricciones de clave primaria en cada tabla.

CREATE TEMP TABLE Tabla_Claves_Primarias AS
SELECT
    table_schema,
    table_name,
    constraint_name
FROM
    information_schema.table_constraints
WHERE
    constraint_type = 'PRIMARY KEY';

-- Crear una vista temporal para listar todas las restricciones de clave foránea en cada tabla.

CREATE TEMP TABLE Tabla_Claves_Foraneas AS
SELECT
    table_schema,
    table_name,
    constraint_name,
    column_name,
    referenced_table_schema,
    referenced_table_name,
    referenced_column_name
FROM
    information_schema.key_column_usage
WHERE
    constraint_name NOT IN (
        SELECT constraint_name FROM Tabla_Claves_Primarias
    );

-- Crear una vista temporal para listar todos los índices en cada tabla.

CREATE TEMP TABLE Tabla_Indices AS
SELECT
    table_schema,
    table_name,
    index_name,
    column_name
FROM
    information_schema.statistics
WHERE
    index_name IS NOT NULL;

-- Seleccionar información de todas las vistas temporales creadas anteriormente y unirlas para obtener un informe detallado de cada tabla.

SELECT
    TRC.table_schema,           -- Esquema de la tabla
    TRC.table_name,             -- Nombre de la tabla
    TRC.registro_cuenta,         -- Número de registros en la tabla
    TCDT.column_name,            -- Nombre de la columna
    TCDT.data_type,              -- Tipo de dato de la columna
    TCP.constraint_name,         -- Nombre de la restricción de clave primaria (si existe)
    TCF.constraint_name AS FK_constraint_name,  -- Nombre de la restricción de clave foránea (si existe)
    TCF.column_name AS FK_column_name,        -- Nombre de la columna de la clave foránea
    TCF.referenced_table_schema AS FK_table_schema, -- Esquema de la tabla referenciada
    TCF.referenced_table_name AS FK_table_name,  -- Nombre de la tabla referenciada
    TCF.referenced_column_name AS FK_column_name, -- Nombre de la columna referenciada
    TI.index_name,               -- Nombre del índice (si existe)
    TI.column_name AS index_column_name -- Nombre de la columna del índice
FROM
    Tabla_Recuento_Registros AS TRC
INNER JOIN
    Tabla_Columnas_Tipos_Datos AS TCDT ON TRC.table_schema = TCDT.table_schema AND TRC.table_name = TCDT.table_name
LEFT JOIN
    Tabla_Claves_Primarias AS TCP ON TRC.table_schema = TCP.table_schema AND TRC.table_name = TCP.table_name
LEFT JOIN
    Tabla_Claves_Foraneas AS TCF ON TRC.table_schema = TCF.table_schema AND TRC.table_name = TCF.table_name
LEFT JOIN
    Tabla_Indices AS TI ON TRC.table_schema = TI.table_schema AND TRC.table_name = TI.table_name
ORDER BY
    TRC.table_schema, TRC.table_name, TCDT.column_name;

-- Eliminar las vistas temporales creadas anteriormente.

DROP TABLE Tabla_Recuento_Registros;
DROP TABLE Tabla_Columnas_Tipos_Datos;
DROP TABLE Tabla_Claves_Primarias;
DROP TABLE Tabla_Claves_Foraneas;
DROP TABLE Tabla_Indices;
```

Este código SQL crea un informe detallado de cada tabla en la base de datos, incluyendo información sobre la cantidad de registros, las columnas y sus tipos de datos, las restricciones de clave primaria y foránea, y los índices.

El código utiliza vistas temporales para almacenar datos intermedios y luego las une para obtener el informe final.

Este código puede ser útil para administradores de bases de datos, desarrolladores y otros profesionales de TI que necesitan obtener información detallada sobre las tablas en una base de datos.