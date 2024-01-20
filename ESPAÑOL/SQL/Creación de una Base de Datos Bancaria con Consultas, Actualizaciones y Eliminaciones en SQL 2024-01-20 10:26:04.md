```sql
-- Creación de base de datos "Banco"
CREATE DATABASE Banco;

-- Selección de la base de datos "Banco"
USE Banco;

-- Creación de tablas

-- Tabla "Clientes"
CREATE TABLE Clientes (
    id_cliente INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(50) NOT NULL,
    apellido VARCHAR(50) NOT NULL,
    dni VARCHAR(8) UNIQUE NOT NULL,
    correo_electronico VARCHAR(50) UNIQUE NOT NULL,
    telefono VARCHAR(15) NOT NULL,
    direccion VARCHAR(100) NOT NULL,
    PRIMARY KEY (id_cliente)
);

-- Tabla "Cuentas"
CREATE TABLE Cuentas (
    id_cuenta INT NOT NULL AUTO_INCREMENT,
    id_cliente INT NOT NULL,
    numero_cuenta VARCHAR(20) UNIQUE NOT NULL,
    tipo_cuenta VARCHAR(20) NOT NULL,
    saldo DECIMAL(10,2) NOT NULL DEFAULT 0.00,
    estado VARCHAR(10) NOT NULL DEFAULT 'Activo',
    fecha_creacion TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    PRIMARY KEY (id_cuenta),
    FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

-- Tabla "Transacciones"
CREATE TABLE Transacciones (
    id_transaccion INT NOT NULL AUTO_INCREMENT,
    id_cuenta INT NOT NULL,
    tipo_transaccion VARCHAR(20) NOT NULL,
    monto DECIMAL(10,2) NOT NULL,
    fecha_transaccion TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
    descripcion VARCHAR(100),
    PRIMARY KEY (id_transaccion),
    FOREIGN KEY (id_cuenta) REFERENCES Cuentas(id_cuenta)
);

-- Inserción de datos iniciales

-- Insertar datos en la tabla "Clientes"
INSERT INTO Clientes (nombre, apellido, dni, correo_electronico, telefono, direccion) VALUES
('Juan', 'García', '12345678', 'juan.garcia@ejemplo.com', '601234567', 'Calle Mayor 123'),
('María', 'López', '87654321', 'maria.lopez@ejemplo.com', '602345678', 'Calle Menor 234'),
('Pedro', 'Sánchez', '98765432', 'pedro.sanchez@ejemplo.com', '603456789', 'Calle Real 345');

-- Insertar datos en la tabla "Cuentas"
INSERT INTO Cuentas (id_cliente, numero_cuenta, tipo_cuenta, saldo) VALUES
(1, 'ES12345678901234567890', 'Cuenta corriente', 1000.00),
(2, 'ES98765432109876543210', 'Cuenta de ahorros', 2000.00),
(3, 'ES23456789012345678901', 'Cuenta nómina', 3000.00);

-- Insertar datos en la tabla "Transacciones"
INSERT INTO Transacciones (id_cuenta, tipo_transaccion, monto, descripcion) VALUES
(1, 'Retiro', -200.00, 'Compra en supermercado'),
(2, 'Depósito', 500.00, 'Ingreso de nómina'),
(3, 'Transferencia', -100.00, 'Transferencia a cuenta ES23456789012345678901');

-- Consultas

-- Obtener el nombre y apellido de los clientes con saldo total superior a 2500 euros
SELECT c.nombre, c.apellido
FROM Clientes c
JOIN Cuentas cu ON c.id_cliente = cu.id_cliente
WHERE cu.saldo > 2500.00;

-- Obtener el número de cuenta y el saldo de las cuentas con tipo "Cuenta corriente"
SELECT cu.numero_cuenta, cu.saldo
FROM Cuentas cu
WHERE cu.tipo_cuenta = 'Cuenta corriente';

-- Obtener la fecha y el monto de las transacciones de tipo "Retiro" realizadas en el último mes
SELECT t.fecha_transaccion, t.monto
FROM Transacciones t
WHERE t.tipo_transaccion = 'Retiro'
AND t.fecha_transaccion >= DATE_SUB(NOW(), INTERVAL 1 MONTH);

-- Actualizaciones

-- Actualizar el saldo de la cuenta ES12345678901234567890 a 5000 euros
UPDATE Cuentas
SET saldo = 5000.00
WHERE numero_cuenta = 'ES12345678901234567890';

-- Dar de baja la cuenta ES98765432109876543210
UPDATE Cuentas
SET estado = 'Inactivo'
WHERE numero_cuenta = 'ES98765432109876543210';

-- Borrar las transacciones anteriores al 1 de enero de 2023
DELETE FROM Transacciones
WHERE fecha_transaccion < '2023-01-01';
```

Este código crea una base de datos llamada "Banco" y tres tablas: "Clientes", "Cuentas" y "Transacciones". A continuación, inserta algunos datos iniciales en las tablas y realiza varias consultas, actualizaciones y borrados.

El código está comentado en español para facilitar su comprensión.