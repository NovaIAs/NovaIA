```sql
-- Crear una tabla de empleados con sus datos personales y laborales

CREATE TABLE Empleados (
    id_empleado INT PRIMARY KEY AUTO_INCREMENT,
    nombre VARCHAR(50) NOT NULL,
    apellido VARCHAR(50) NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    fecha_nacimiento DATE NOT NULL,
    sexo CHAR(1) CHECK (sexo IN ('M', 'F')), -- M para masculino, F para femenino
    telefono VARCHAR(20) UNIQUE NOT NULL,
    direccion VARCHAR(255) NOT NULL,
    fecha_contratacion DATE NOT NULL,
    cargo VARCHAR(50) NOT NULL,
    salario DECIMAL(10, 2) NOT NULL
);

-- Insertar datos de ejemplo en la tabla de empleados

INSERT INTO Empleados (nombre, apellido, email, fecha_nacimiento, sexo, telefono, direccion, fecha_contratacion, cargo, salario) VALUES
('Juan', 'Perez', 'juan.perez@ejemplo.com', '1990-01-01', 'M', '555-123-4567', 'Calle Principal 123, Ciudad', '2023-01-01', 'Gerente', 5000.00),
('Maria', 'Garcia', 'maria.garcia@ejemplo.com', '1992-02-02', 'F', '555-234-5678', 'Calle Secundaria 456, Ciudad', '2023-02-02', 'Contadora', 4000.00),
('Pedro', 'Lopez', 'pedro.lopez@ejemplo.com', '1994-03-03', 'M', '555-345-6789', 'Calle Terciaria 789, Ciudad', '2023-03-03', 'Vendedor', 3000.00),
('Ana', 'Sanchez', 'ana.sanchez@ejemplo.com', '1996-04-04', 'F', '555-456-7890', 'Calle Cuaternaria 1011, Ciudad', '2023-04-04', 'Asistente Administrativa', 2500.00);

-- Crear una tabla de departamentos con sus datos

CREATE TABLE Departamentos (
    id_departamento INT PRIMARY KEY AUTO_INCREMENT,
    nombre VARCHAR(50) NOT NULL,
    ubicacion VARCHAR(50) NOT NULL
);

-- Insertar datos de ejemplo en la tabla de departamentos

INSERT INTO Departamentos (nombre, ubicacion) VALUES
('Ventas', 'Ciudad'),
('Contabilidad', 'Ciudad'),
('Recursos Humanos', 'Ciudad'),
('Atención al Cliente', 'Ciudad');

-- Crear una tabla de asignación de empleados a departamentos

CREATE TABLE EmpleadosDepartamentos (
    id_empleado INT NOT NULL,
    id_departamento INT NOT NULL,
    FOREIGN KEY (id_empleado) REFERENCES Empleados(id_empleado),
    FOREIGN KEY (id_departamento) REFERENCES Departamentos(id_departamento)
);

-- Insertar datos de ejemplo en la tabla de asignación de empleados a departamentos

INSERT INTO EmpleadosDepartamentos (id_empleado, id_departamento) VALUES
(1, 1), -- Juan Perez en el departamento de Ventas
(2, 2), -- Maria Garcia en el departamento de Contabilidad
(3, 3), -- Pedro Lopez en el departamento de Recursos Humanos
(4, 4); -- Ana Sanchez en el departamento de Atención al Cliente

-- Crear una consulta para mostrar los datos de los empleados junto con el nombre de su departamento

SELECT 
    e.id_empleado,
    e.nombre,
    e.apellido,
    e.email,
    e.fecha_nacimiento,
    e.sexo,
    e.telefono,
    e.direccion,
    e.fecha_contratacion,
    e.cargo,
    e.salario,
    d.nombre AS departamento
FROM 
    Empleados e
INNER JOIN 
    Departamentos d ON e.id_departamento = d.id_departamento;

EXPLICACIÓN:

Este código crea tres tablas en una base de datos SQL: Empleados, Departamentos y EmpleadosDepartamentos.

La tabla Empleados almacena los datos personales y laborales de los empleados, como su nombre, apellido, correo electrónico, fecha de nacimiento, sexo, teléfono, dirección, fecha de contratación, cargo y salario.

La tabla Departamentos almacena los datos de los departamentos de la empresa, como su nombre y ubicación.

La tabla EmpleadosDepartamentos asigna a los empleados a sus respectivos departamentos.

Después de crear las tablas, el código inserta datos de ejemplo en ellas, incluyendo información sobre cuatro empleados y cuatro departamentos.

Finalmente, el código incluye una consulta SQL que recupera los datos de los empleados junto con el nombre de su departamento. Esta consulta utiliza una combinación interna (INNER JOIN) entre las tablas Empleados y Departamentos para unir los datos de ambas tablas.

Este código es complejo porque crea múltiples tablas, inserta datos de ejemplo y utiliza una combinación interna en una consulta SQL. Es poco probable que este código se repita exactamente en otro lugar, ya que se ha creado para un escenario específico.