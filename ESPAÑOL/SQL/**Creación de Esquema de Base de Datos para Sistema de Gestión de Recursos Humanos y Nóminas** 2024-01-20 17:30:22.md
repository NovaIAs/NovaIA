```sql
-- Creación de una tabla de Empleados con campos básicos e información detallada

CREATE TABLE Empleados (
    IdEmpleado INT PRIMARY KEY,
    Nombres VARCHAR(50) NOT NULL,
    Apellidos VARCHAR(50) NOT NULL,
    FechaNacimiento DATE,
    LugarNacimiento VARCHAR(50),
    Dirección VARCHAR(100),
    Ciudad VARCHAR(50),
    Estado VARCHAR(50),
    CódigoPostal VARCHAR(10),
    Teléfono VARCHAR(15),
    CorreoElectrónico VARCHAR(50) UNIQUE,
    Cargo VARCHAR(50),
    Departamento VARCHAR(50),
    FechaContratación DATE,
    Salario DECIMAL(10, 2),
    Bonificación DECIMAL(10, 2),
    Comisiones DECIMAL(10, 2),
    Deducciones DECIMAL(10, 2),
    NetoPagar DECIMAL(10, 2),
    EstadoCivil VARCHAR(10),
    NúmeroHijos INT,
    SeguroMédico BOOLEAN,
    SeguroDental BOOLEAN,
    SeguroDeVida BOOLEAN
);

-- Creación de una tabla de Departamentos con información básica y jerarquía

CREATE TABLE Departamentos (
    IdDepartamento INT PRIMARY KEY,
    NombreDepartamento VARCHAR(50) NOT NULL,
    Gerente VARCHAR(50),
    Ubicación VARCHAR(50),
    Presupuesto DECIMAL(10, 2),
    Gastos DECIMAL(10, 2),
    Beneficio DECIMAL(10, 2)
);

-- Creación de una tabla de Nóminas con información de pago de empleados

CREATE TABLE Nóminas (
    IdNómina INT PRIMARY KEY,
    IdEmpleado INT NOT NULL,
    FechaPago DATE,
    HorasTrabajadas INT,
    TasaPorHora DECIMAL(10, 2),
    SueldoBruto DECIMAL(10, 2),
    Impuestos DECIMAL(10, 2),
    TotalDeducciones DECIMAL(10, 2),
    NetoPagar DECIMAL(10, 2),
    MétodoPago VARCHAR(20),
    NúmeroCuenta VARCHAR(20)
);

-- Creación de una tabla de Asistencias con información de asistencia de empleados

CREATE TABLE Asistencias (
    IdAsistencia INT PRIMARY KEY,
    IdEmpleado INT NOT NULL,
    Fecha Asistencia DATE,
    HoraEntrada TIME,
    HoraSalida TIME,
    TotalHorasTrabajadas DECIMAL(10, 2),
    HorasExtra DECIMAL(10, 2),
    HorasFalta DECIMAL(10, 2),
    Justificación VARCHAR(255)
);

-- Creación de una tabla de Proyectos con información de proyectos en curso

CREATE TABLE Proyectos (
    IdProyecto INT PRIMARY KEY,
    NombreProyecto VARCHAR(50) NOT NULL,
    Descripción VARCHAR(255),
    FechaInicio DATE,
    FechaFin DATE,
    Presupuesto DECIMAL(10, 2),
    Gastos DECIMAL(10, 2),
    Beneficio DECIMAL(10, 2),
    Estado VARCHAR(20)
);

-- Creación de una tabla de Tareas con información de tareas asignadas a empleados

CREATE TABLE Tareas (
    IdTarea INT PRIMARY KEY,
    IdProyecto INT NOT NULL,
    NombreTarea VARCHAR(50) NOT NULL,
    Descripción VARCHAR(255),
    FechaAsignación DATE,
    FechaEntrega DATE,
    Estado VARCHAR(20),
    Prioridad VARCHAR(10),
    Responsable VARCHAR(50)
);

-- Creación de una tabla de Clientes con información de clientes de la empresa

CREATE TABLE Clientes (
    IdCliente INT PRIMARY KEY,
    NombreCliente VARCHAR(50) NOT NULL,
    Dirección VARCHAR(100),
    Ciudad VARCHAR(50),
    Estado VARCHAR(50),
    CódigoPostal VARCHAR(10),
    Teléfono VARCHAR(15),
    CorreoElectrónico VARCHAR(50) UNIQUE,
    TipoCliente VARCHAR(20),
    Crédito DECIMAL(10, 2),
    SaldoPendiente DECIMAL(10, 2)
);

-- Creación de una tabla de Facturas con información de facturas emitidas a clientes

CREATE TABLE Facturas (
    IdFactura INT PRIMARY KEY,
    IdCliente INT NOT NULL,
    FechaFactura DATE,
    NúmeroFactura VARCHAR(20) UNIQUE,
    ImporteTotal DECIMAL(10, 2),
    Impuestos DECIMAL(10, 2),
    Descuentos DECIMAL(10, 2),
    NetoPagar DECIMAL(10, 2),
    MétodoPago VARCHAR(20),
    NúmeroCuenta VARCHAR(20)
);

-- Creación de una tabla de Productos con información de productos vendidos

CREATE TABLE Productos (
    IdProducto INT PRIMARY KEY,
    NombreProducto VARCHAR(50) NOT NULL,
    Descripción VARCHAR(255),
    PrecioUnitario DECIMAL(10, 2),
    CantidadEnStock INT,
    Categoría VARCHAR(50),
    Proveedor VARCHAR(50),
    FechaCaducidad DATE
);

-- Creación de una tabla de Pedidos con información de pedidos realizados por clientes

CREATE TABLE Pedidos (
    IdPedido INT PRIMARY KEY,
    IdCliente INT NOT NULL,
    FechaPedido DATE,
