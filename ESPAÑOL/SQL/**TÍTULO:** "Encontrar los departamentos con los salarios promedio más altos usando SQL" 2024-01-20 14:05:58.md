```sql
-- Crear una tabla temporal con los datos de los empleados y sus salarios
CREATE TEMP TABLE EmpleadosSalarios AS
SELECT
    e.nombre AS NombreEmpleado,
    e.departamento AS DepartamentoEmpleado,
    s.salario AS SalarioEmpleado
FROM
    Empleados e
JOIN
    Salarios s ON e.id = s.id_empleado;

-- Crear una tabla temporal con los datos de los departamentos y los salarios promedio de sus empleados
CREATE TEMP TABLE DepartamentosSalariosPromedio AS
SELECT
    e.departamento AS DepartamentoEmpleado,
    AVG(s.salario) AS SalarioPromedioDepartamento
FROM
    EmpleadosSalarios e
JOIN
    Departamentos d ON e.departamento = d.id
GROUP BY
    e.departamento;

-- Seleccionar los departamentos con los salarios promedio más altos
SELECT
    d.nombre AS Departamento,
    ds.SalarioPromedioDepartamento
FROM
    Departamentos d
JOIN
    DepartamentosSalariosPromedio ds ON d.id = ds.DepartamentoEmpleado
ORDER BY
    ds.SalarioPromedioDepartamento DESC
LIMIT 5;

-- Mostrar los resultados
SELECT * FROM resultado;

-- Eliminar las tablas temporales
DROP TABLE EmpleadosSalarios;
DROP TABLE DepartamentosSalariosPromedio;
```

Explicación:

* La primera consulta crea una tabla temporal llamada `EmpleadosSalarios` que contiene los datos de los empleados y sus salarios.
* La segunda consulta crea una tabla temporal llamada `DepartamentosSalariosPromedio` que contiene los datos de los departamentos y los salarios promedio de sus empleados.
* La tercera consulta selecciona los departamentos con los salarios promedio más altos.
* La cuarta consulta muestra los resultados de la tercera consulta.
* La quinta consulta elimina las tablas temporales `EmpleadosSalarios` y `DepartamentosSalariosPromedio`.