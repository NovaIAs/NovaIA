```sql
--Crear una base de datos llamada "Banco"
CREATE DATABASE Banco;

--Seleccionar la base de datos "Banco"
USE Banco;

--Crear una tabla llamada "Clientes" con los siguientes columnas:
-- - id_cliente:    Número de identificación único del cliente (clave principal)
-- - nombre:        Nombre del cliente
-- - apellidos:      Apellidos del cliente
-- - direccion:     Dirección del cliente
-- - telefono:      Número de teléfono del cliente
-- - correo_electronico: Correo electrónico del cliente
-- - fecha_nacimiento:  Fecha de nacimiento del cliente
CREATE TABLE Clientes (
    id_cliente INTEGER PRIMARY KEY AUTO_INCREMENT,
    nombre VARCHAR(50) NOT NULL,
    apellidos VARCHAR(50) NOT NULL,
    direccion VARCHAR(255) NOT NULL,
    telefono CHAR(10) UNIQUE,
    correo_electronico VARCHAR(50) UNIQUE,
    fecha_nacimiento DATE
);

--Crear una tabla llamada "Cuentas" con los siguientes columnas:
-- - id_cuenta:     Número de identificación único de la cuenta (clave primaria)
-- - id_cliente:    Número de identificación del cliente al que pertenece la cuenta
-- - numero_cuenta:    Número de cuenta
-- - saldo:           Saldo actual de la cuenta
-- - tipo_cuenta:     Tipo de cuenta (ahorros, corriente, etc.)
CREATE TABLE Cuentas (
    id_cuenta INTEGER PRIMARY KEY AUTO_INCREMENT,
    id_cliente INTEGER NOT NULL,
    numero_cuenta VARCHAR(20) UNIQUE,
    saldo DECIMAL(10,2) DEFAULT 0.00,
    tipo_cuenta VARCHAR(20) NOT NULL,
    CONSTRAINT FK_Cliente FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

--Crear una tabla llamada "Transacciones" con los siguientes columnas:
-- - id_transaccion:  Número de identificación único de la transacción (clave primaria)
-- - id_cuenta:      Número de identificación de la cuenta donde se realizó la transacción
-- - fecha_transaccion: Fecha en que se realizó la transacción
-- - tipo_transaccion: Tipo de transacción (depósito, retiro, transferencia, etc.)
-- - monto:           Monto de la transacción
CREATE TABLE Transacciones (
    id_transaccion INTEGER PRIMARY KEY AUTO_INCREMENT,
    id_cuenta INTEGER NOT NULL,
    fecha_transaccion DATETIME,
    tipo_transaccion VARCHAR(20) NOT NULL,
    monto DECIMAL(10,2) NOT NULL,
    CONSTRAINT FK_Cuenta FOREIGN KEY (id_cuenta) REFERENCES Cuentas(id_cuenta)
);

--Insertar algunos datos en la tabla "Clientes"
INSERT INTO Clientes (nombre, apellidos, direccion, telefono, correo_electronico, fecha_nacimiento) VALUES
('Juan', 'García', 'Calle 123, Madrid', '912345678', 'juan.garcia@ejemplo.com', '1980-01-01'),
('María', 'Pérez', 'Calle 456, Barcelona', '932345678', 'maria.perez@ejemplo.com', '1985-07-15'),
('Pedro', 'López', 'Calle 789, Valencia', '952345678', 'pedro.lopez@ejemplo.com', '1990-03-08');

--Insertar algunos datos en la tabla "Cuentas"
INSERT INTO Cuentas (id_cliente, numero_cuenta, saldo, tipo_cuenta) VALUES
(1, 'ES12345678901234567890', 1000.00, 'Ahorros'),
(2, 'ES98765432109876543210', 2000.00, 'Corriente'),
(3, 'ES01234567890123456789', 3000.00, 'Ahorros');

--Insertar algunos datos en la tabla "Transacciones"
INSERT INTO Transacciones (id_cuenta, fecha_transaccion, tipo_transaccion, monto) VALUES
(1, '2023-02-13 10:00:00', 'Depósito', 500.00),
(2, '2023-02-14 11:00:00', 'Retiro', 300.00),
(3, '2023-02-15 12:00:00', 'Transferencia', 1000.00);

--Mostrar todos los clientes de la base de datos
SELECT * FROM Clientes;

--Mostrar todos los clientes de la base de datos ordenados por su nombre
SELECT * FROM Clientes ORDER BY nombre;

--Mostrar todos los clientes de la base de datos con un saldo mayor a 2000 euros
SELECT * FROM Clientes WHERE saldo > 2000;

--Mostrar el nombre y el saldo de todos los clientes de la base de datos
SELECT nombre, saldo FROM Clientes;

--Mostrar el nombre y el saldo de todos los clientes de la base de datos ordenados por su nombre
SELECT nombre, saldo FROM Clientes ORDER BY nombre;

--Mostrar el nombre y el saldo de todos los clientes de la base de datos con un saldo mayor a 2000 euros
SELECT nombre, saldo FROM Clientes WHERE saldo > 2000;

--Mostrar el nombre, el apellido y el saldo de todos los clientes de la base de datos
SELECT nombre, apellidos, saldo FROM Clientes;

--Mostrar el nombre, el apellido y el saldo de todos los clientes de la base de datos ordenados por su nombre
SELECT nombre, apellidos, saldo FROM Clientes ORDER BY nombre;

--Mostrar el nombre, el apellido y el saldo de todos los clientes de la base de datos con un saldo mayor a 2000 euros
SELECT nombre, apellidos, saldo FROM Clientes WHERE saldo > 2000;

--Mostrar el nombre, el apellido y el saldo de todos los clientes de la base de datos con un saldo mayor a 2000 euros ordenados por su nombre
SELECT nombre, apellidos, saldo FROM Clientes WHERE saldo > 2000 ORDER BY nombre;
```

Explicación del código:

* La primera parte del código crea la base de datos "Banco" y la selecciona para su uso.
* La segunda parte del código crea las tres tablas necesarias para la base de datos: "Clientes", "Cuentas" y "Transacciones".
* La tercera parte del código inserta algunos datos de ejemplo en las tres tablas.
* La cuarta parte del código muestra los datos de las tres tablas en diferentes formatos.

El código está escrito en español y utiliza la sintaxis de SQL para crear las tablas, insertar los datos y mostrar los resultados.