```sql
-- Crear un procedimiento almacenado para insertar nuevos empleados en la base de datos.
CREATE PROCEDURE insertar_empleados (
    @nombre VARCHAR(50),
    @apellido VARCHAR(50),
    @fecha_nacimiento DATE,
    @salario DECIMAL(10, 2),
    @departamento INT
)
AS
BEGIN
    -- Insertar los datos del nuevo empleado en la tabla Empleados.
    INSERT INTO Empleados (nombre, apellido, fecha_nacimiento, salario, departamento)
    VALUES (@nombre, @apellido, @fecha_nacimiento, @salario, @departamento);
END;

-- Crear una función para calcular el salario total de un empleado.
CREATE FUNCTION calcular_salario_total (
    @salario DECIMAL(10, 2),
    @bono DECIMAL(10, 2)
)
RETURNS DECIMAL(10, 2)
AS
BEGIN
    -- Calcular el salario total del empleado sumando el salario base y el bono.
    RETURN @salario + @bono;
END;

-- Crear un disparador para actualizar el salario total de un empleado cuando se modifica su salario base o su bono.
CREATE TRIGGER actualizar_salario_total
ON Empleados
FOR UPDATE
AS
BEGIN
    -- Actualizar el salario total del empleado llamando a la función calcular_salario_total.
    UPDATE Empleados
    SET salario_total = calcular_salario_total(salario, bono)
    WHERE id_empleado = OLD.id_empleado;
END;

-- Crear una vista para mostrar el nombre, el apellido y el salario total de los empleados.
CREATE VIEW EmpleadosSalarioTotal
AS
SELECT nombre, apellido, salario_total
FROM Empleados;

-- Crear una consulta para obtener el nombre, el apellido y el salario total de los empleados que trabajan en un departamento específico.
SELECT nombre, apellido, salario_total
FROM EmpleadosSalarioTotal
WHERE departamento = 10;

-- Crear un índice en la columna departamento de la tabla Empleados para mejorar el rendimiento de las consultas que filtran por departamento.
CREATE INDEX IX_Empleados_Departamento ON Empleados (departamento);
```

Explicación del código:

* El procedimiento almacenado `insertar_empleados` se utiliza para insertar nuevos empleados en la base de datos.
* La función `calcular_salario_total` se utiliza para calcular el salario total de un empleado sumando el salario base y el bono.
* El disparador `actualizar_salario_total` se utiliza para actualizar el salario total de un empleado cuando se modifica su salario base o su bono.
* La vista `EmpleadosSalarioTotal` se utiliza para mostrar el nombre, el apellido y el salario total de los empleados.
* La consulta `SELECT nombre, apellido, salario_total FROM EmpleadosSalarioTotal WHERE departamento = 10` se utiliza para obtener el nombre, el apellido y el salario total de los empleados que trabajan en el departamento 10.
* El índice `IX_Empleados_Departamento` se utiliza para mejorar el rendimiento de las consultas que filtran por departamento.