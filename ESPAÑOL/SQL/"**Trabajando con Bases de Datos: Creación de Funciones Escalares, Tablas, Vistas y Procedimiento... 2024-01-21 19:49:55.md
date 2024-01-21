```sql
-- Crear una función escalar para calcular el área de triángulos rectángulos.
CREATE FUNCTION calcular_area_triangulo_rectangulo(@base DECIMAL(10, 2), @altura DECIMAL(10, 2)) RETURNS DECIMAL(10, 2)
AS
BEGIN
    -- Calcular el área del triángulo rectángulo.
    DECLARE @area DECIMAL(10, 2) = (@base * @altura) / 2;

    -- Devolver el área calculada.
    RETURN @area;
END;

-- Crear una tabla de empleados con los siguientes campos:
-- - IdEmpleado: Identificador único del empleado.
-- - Nombre: Nombre del empleado.
-- - Apellidos: Apellidos del empleado.
-- - FechaNacimiento: Fecha de nacimiento del empleado.
-- - Salario: Salario del empleado.
CREATE TABLE Empleados (
    IdEmpleado INT NOT NULL IDENTITY(1, 1) PRIMARY KEY,
    Nombre VARCHAR(50) NOT NULL,
    Apellidos VARCHAR(100) NOT NULL,
    FechaNacimiento DATE NOT NULL,
    Salario DECIMAL(10, 2) NOT NULL
);

-- Insertar datos en la tabla de empleados.
INSERT INTO Empleados (Nombre, Apellidos, FechaNacimiento, Salario) VALUES
('Juan', 'García', '1990-01-01', 1000.00),
('María', 'Pérez', '1995-07-15', 1500.00),
('Pedro', 'López', '1985-12-24', 2000.00),
('Ana', 'Fernández', '1970-03-08', 2500.00),
('José', 'Martínez', '1965-09-12', 3000.00);

-- Crear una vista que muestre el nombre completo de los empleados y su salario.
CREATE VIEW EmpleadosCompletos AS
SELECT
    IdEmpleado,
    Nombre || ' ' || Apellidos AS NombreCompleto,
    Salario
FROM
    Empleados;

-- Crear un procedimiento almacenado para aumentar el salario de los empleados en un porcentaje especificado.
CREATE PROCEDURE aumentar_salario (@porcentaje_aumento DECIMAL(10, 2))
AS
BEGIN
    -- Actualizar los salarios de los empleados.
    UPDATE Empleados
    SET Salario = Salario * (1 + (@porcentaje_aumento / 100));
END;

-- Llamada al procedimiento almacenado para aumentar el salario de los empleados en un 10%.
EXEC aumentar_salario 10;

-- Crear una consulta que muestre el nombre completo, el salario y el área de un triángulo rectángulo para cada empleado.
SELECT
    ec.NombreCompleto,
    ec.Salario,
    calcular_area_triangulo_rectangulo(10, 5) AS AreaTriangulo
FROM
    EmpleadosCompletos ec;
```

Explicación del código:

* La función escalar `calcular_area_triangulo_rectangulo` calcula el área de un triángulo rectángulo dados su base y su altura.
* La tabla `Empleados` almacena información sobre los empleados, como su nombre, apellidos, fecha de nacimiento y salario.
* La vista `EmpleadosCompletos` muestra el nombre completo y el salario de los empleados.
* El procedimiento almacenado `aumentar_salario` aumenta el salario de los empleados en un porcentaje especificado.
* La consulta final muestra el nombre completo, el salario y el área de un triángulo rectángulo para cada empleado.