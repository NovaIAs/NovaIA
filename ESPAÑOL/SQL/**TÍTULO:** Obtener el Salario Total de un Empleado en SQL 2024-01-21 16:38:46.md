```sql
-- Crear una función para obtener el salario total de un empleado basado en su identificación.

CREATE FUNCTION ObtenerSalarioTotal(@IdEmpleado int)
RETURNS money
AS
BEGIN
    -- Obtener el salario base del empleado.
    DECLARE @SalarioBase money = (
        SELECT SalarioBase
        FROM Empleados
        WHERE IdEmpleado = @IdEmpleado
    );

    -- Obtener las horas extra trabajadas por el empleado.
    DECLARE @HorasExtra int = (
        SELECT HorasExtra
        FROM Empleados
        WHERE IdEmpleado = @IdEmpleado
    );

    -- Obtener el pago por hora extra.
    DECLARE @PagoHoraExtra money = (
        SELECT PagoHoraExtra
        FROM Empleados
        WHERE IdEmpleado = @IdEmpleado
    );

    -- Calcular el salario total del empleado.
    DECLARE @SalarioTotal money = @SalarioBase + (@HorasExtra * @PagoHoraExtra);

    -- Devolver el salario total del empleado.
    RETURN @SalarioTotal;
END;

-- Utilizar la función para obtener el salario total de un empleado con IdEmpleado = 1.

DECLARE @SalarioTotal money = ObtenerSalarioTotal(1);

-- Mostrar el salario total del empleado.

SELECT @SalarioTotal AS 'Salario Total';

```

**Explicación:**

* La función `ObtenerSalarioTotal` toma como parámetro el `IdEmpleado` y devuelve el salario total del empleado.
* La función primero obtiene el salario base, las horas extra trabajadas y el pago por hora extra del empleado.
* Luego calcula el salario total del empleado sumando el salario base y el pago por horas extra.
* La función finalmente devuelve el salario total del empleado.
* El código utiliza la función `ObtenerSalarioTotal` para obtener el salario total de un empleado con `IdEmpleado` igual a 1.
* El salario total del empleado se muestra en la consola.