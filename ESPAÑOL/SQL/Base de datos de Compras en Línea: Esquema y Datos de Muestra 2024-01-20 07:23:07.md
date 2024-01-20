```sql
-- Base de datos: Compras en línea

-- Crear la base de datos
CREATE DATABASE Compras_En_Línea;

-- Usar la base de datos
USE Compras_En_Línea;

-- Crear la tabla "Productos"
CREATE TABLE Productos (
    id_producto INT NOT NULL AUTO_INCREMENT,
    nombre_producto VARCHAR(255) NOT NULL,
    descripcion VARCHAR(2000) NOT NULL,
    precio DECIMAL(10,2) NOT NULL,
    stock INT NOT NULL,
    PRIMARY KEY (id_producto)
);

-- Insertar datos en la tabla "Productos"
INSERT INTO Productos (nombre_producto, descripcion, precio, stock) VALUES
('iPhone 14 Pro Max', 'El último teléfono inteligente de Apple con una pantalla de 6,7 pulgadas, un procesador A16 Bionic y un sistema de cámara de triple lente.', 1099.00, 100),
('Samsung Galaxy S23 Ultra', 'El teléfono inteligente más avanzado de Samsung con una pantalla de 6,8 pulgadas, un procesador Snapdragon 8 Gen 2 y un sistema de cámara de cuádruple lente.', 1199.00, 80),
('Google Pixel 7 Pro', 'El teléfono inteligente de Google con una pantalla de 6,7 pulgadas, un procesador Tensor G2 y un sistema de cámara de triple lente.', 899.00, 90),
('OnePlus 11', 'El teléfono inteligente de OnePlus con una pantalla de 6,7 pulgadas, un procesador Snapdragon 8 Gen 2 y un sistema de cámara de triple lente.', 799.00, 110),
('Xiaomi 13 Pro', 'El teléfono inteligente de Xiaomi con una pantalla de 6,7 pulgadas, un procesador Snapdragon 8 Gen 2 y un sistema de cámara de triple lente.', 749.00, 120);

-- Crear la tabla "Clientes"
CREATE TABLE Clientes (
    id_cliente INT NOT NULL AUTO_INCREMENT,
    nombre_cliente VARCHAR(255) NOT NULL,
    direccion VARCHAR(500) NOT NULL,
    telefono VARCHAR(50) NOT NULL,
    correo_electronico VARCHAR(255) NOT NULL,
    PRIMARY KEY (id_cliente)
);

-- Insertar datos en la tabla "Clientes"
INSERT INTO Clientes (nombre_cliente, direccion, telefono, correo_electronico) VALUES
('Juan García', 'Calle Mayor 123, Madrid', '911223344', 'juangarcia@ejemplo.com'),
('María López', 'Avenida del Sol 456, Barcelona', '933445566', 'marialopez@ejemplo.com'),
('Pedro Sánchez', 'Plaza de España 789, Sevilla', '955667788', 'pedrosanchez@ejemplo.com'),
('Ana Fernández', 'Calle de la Luna 1011, Valencia', '966778899', 'anafernandez@ejemplo.com'),
('José Rodríguez', 'Paseo de la Castellana 1213, Madrid', '977889900', 'joserodriguez@ejemplo.com');

-- Crear la tabla "Pedidos"
CREATE TABLE Pedidos (
    id_pedido INT NOT NULL AUTO_INCREMENT,
    id_cliente INT NOT NULL,
    fecha_pedido DATE NOT NULL,
    estado_pedido VARCHAR(50) NOT NULL,
    PRIMARY KEY (id_pedido),
    FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

-- Insertar datos en la tabla "Pedidos"
INSERT INTO Pedidos (id_cliente, fecha_pedido, estado_pedido) VALUES
(1, '2023-03-08', 'Procesando'),
(2, '2023-03-10', 'Enviado'),
(3, '2023-03-12', 'Entregado'),
(4, '2023-03-15', 'Procesando'),
(5, '2023-03-17', 'Enviado');

-- Crear la tabla "Líneas_de_Pedido"
CREATE TABLE Líneas_de_Pedido (
    id_linea_de_pedido INT NOT NULL AUTO_INCREMENT,
    id_pedido INT NOT NULL,
    id_producto INT NOT NULL,
    cantidad INT NOT NULL,
    precio_unitario DECIMAL(10,2) NOT NULL,
    PRIMARY KEY (id_linea_de_pedido),
    FOREIGN KEY (id_pedido) REFERENCES Pedidos(id_pedido),
    FOREIGN KEY (id_producto) REFERENCES Productos(id_producto)
);

-- Insertar datos en la tabla "Líneas_de_Pedido"
INSERT INTO Líneas_de_Pedido (id_pedido, id_producto, cantidad, precio_unitario) VALUES
(1, 1, 2, 1099.00),
(1, 3, 1, 899.00),
(2, 2, 1, 1199.00),
(2, 4, 2, 799.00),
(3, 5, 3, 749.00),
(4, 1, 1, 1099.00),
(4, 5, 1, 749.00),
(5, 2, 2, 1199.00),
(5, 3, 1, 899.00);

-- Crear la tabla "Pagos"
CREATE TABLE Pagos (
    id_pago INT NOT NULL AUTO_INCREMENT,
    id_pedido INT NOT NULL,
    fecha_pago DATE NOT NULL,
    metodo_pago VARCHAR(50) NOT NULL,
    importe_pago DECIMAL(10,2) NOT NULL,
    PRIMARY KEY (id_pago),
    FOREIGN KEY (id_pedido) REFERENCES Pedidos(id_pedido)
);

-- Insertar datos en la tabla "Pagos"
INSERT INTO Pagos (id_pedido, fecha_pago, metodo_pago, importe_pago) VALUES
(1, '2023-03-09', 'Tarjeta de crédito', 1998.00),
(2, '2023-03-11', 'Transferencia bancaria', 1199.00),
(3, '2023-03-13', 'PayPal', 2247.00),
(4, '2023-03-16', 'Tarjeta de crédito', 1099.00),
(5, '2023-03-18', 'Transferencia bancaria', 2398.00);

-- Mostrar todos los datos de la base de datos
SELECT * FROM Productos;

SELECT * FROM Clientes;

SELECT * FROM Pedidos;

SELECT * FROM Líneas_de_Pedido;

SELECT * FROM Pagos;
```

Explicación del código:

* Se crea la base de datos "Compras_En_Línea" y se selecciona como la base de datos actual.
* Se crean las tablas "Productos", "Clientes", "Pedidos", "Líneas_de_Pedido" y "Pagos", con sus respectivas columnas y restricciones.
* Se insertan datos de muestra en cada una de las tablas.
* Se ejecutan consultas para mostrar todos los datos de las tablas.

Este código es complejo porque crea una base de datos completa con múltiples tablas y relaciones entre ellas. También incluye datos de muestra en cada tabla. El código está bien comentado y es fácil de entender.