```
-- Definición de un procedimiento almacenado que recibe dos parámetros:
-- @nombre (tipo VARCHAR) y @edad (tipo INT)
CREATE PROCEDURE ObtenerDetallesPersona(
    @nombre VARCHAR(100),
    @edad INT
)
AS
BEGIN
    -- Declaración de variables locales
    DECLARE @cadena VARCHAR(255)

    -- Concatenación de los valores de los parámetros en una cadena
    SET @cadena = 'Persona: ' + @nombre + ', Edad: ' + CAST(@edad AS VARCHAR)

    -- Impresión de la cadena resultante
    PRINT @cadena
END

-- Ejecución del procedimiento almacenado con valores específicos para los parámetros
EXEC ObtenerDetallesPersona 'Juan Pérez', 30

-- Definición de una función escalar que recibe un parámetro:
-- @texto (tipo VARCHAR)
CREATE FUNCTION ContarPalabras(@texto VARCHAR(MAX))
RETURNS INT
AS
BEGIN
    -- Eliminación de espacios en blanco al inicio y al final del texto
    SET @texto = LTRIM(RTRIM(@texto))

    -- Sustitución de múltiples espacios en blanco consecutivos por un solo espacio
    SET @texto = REPLACE(@texto, '  ', ' ')

    -- División del texto en palabras
    DECLARE @palabras TABLE (Palabra VARCHAR(100))
    INSERT INTO @palabras (Palabra)
    SELECT Palabra FROM dbo.fn_Split(@texto, ' ')

    -- Cuenta del número de palabras
    RETURN (SELECT COUNT(*) FROM @palabras)
END

-- Obtención del número de palabras en una cadena de texto específica
SELECT ContarPalabras('Hola mundo, esta es una prueba') AS NumeroPalabras

-- Definición de un activador DML que se ejecuta antes de la inserción de datos en la tabla [dbo].[Personas]
CREATE TRIGGER InsertarFechaRegistro ON [dbo].[Personas]
FOR INSERT
AS
BEGIN
    -- Obtención de la fecha y hora actual
    DECLARE @fechaHoraActual DATETIME
    SET @fechaHoraActual = GETDATE()

    -- Actualización de la columna [FechaRegistro] con la fecha y hora actuales
    UPDATE [dbo].[Personas]
    SET FechaRegistro = @fechaHoraActual
    WHERE INSERTED.IdPersona = SCOPE_IDENTITY()
END

-- Inserción de un nuevo registro en la tabla [dbo].[Personas]
INSERT INTO [dbo].[Personas] (Nombre, Apellido, Email)
VALUES ('Juan', 'Pérez', 'juan.perez@ejemplo.com')

-- Definición de una vista que muestra los nombres y apellidos de las personas de la tabla [dbo].[Personas]
CREATE VIEW vw_PersonasNombreCompleto AS
SELECT Nombre, Apellido
FROM [dbo].[Personas]

-- Obtención de los nombres y apellidos de las personas en orden alfabético
SELECT Nombre, Apellido
FROM vw_PersonasNombreCompleto
ORDER BY Nombre, Apellido
```

Explicación:

* El código crea un procedimiento almacenado llamado `ObtenerDetallesPersona` que recibe dos parámetros: `@nombre` y `@edad`, y los imprime en una cadena concatenada.

* También define una función escalar llamada `ContarPalabras` que recibe un parámetro de texto y cuenta el número de palabras en el texto.

* A continuación, crea un activador DML llamado `InsertarFechaRegistro` que se ejecuta antes de la inserción de datos en la tabla `[dbo].[Personas]` y actualiza la columna `FechaRegistro` con la fecha y hora actuales.

* Después, crea una vista llamada `vw_PersonasNombreCompleto` que muestra los nombres y apellidos de las personas de la tabla `[dbo].[Personas]`.

* Por último, realiza una consulta para obtener los nombres y apellidos de las personas en orden alfabético.