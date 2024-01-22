```sql
-- Creación de la base de datos "Tienda"
CREATE DATABASE Tienda;

-- Selección de la base de datos "Tienda"
USE Tienda;

-- Creación de la tabla "Productos"
CREATE TABLE Productos (
    id_producto INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    descripcion TEXT,
    precio DECIMAL(10, 2) NOT NULL,
    stock INT NOT NULL DEFAULT 0,
    PRIMARY KEY (id_producto)
);

-- Creación de la tabla "Categorias"
CREATE TABLE Categorias (
    id_categoria INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    descripcion TEXT,
    PRIMARY KEY (id_categoria)
);

-- Creación de la tabla "Productos_Categorias"
CREATE TABLE Productos_Categorias (
    id_producto INT NOT NULL,
    id_categoria INT NOT NULL,
    PRIMARY KEY (id_producto, id_categoria),
    FOREIGN KEY (id_producto) REFERENCES Productos(id_producto),
    FOREIGN KEY (id_categoria) REFERENCES Categorias(id_categoria)
);

-- Creación de la tabla "Clientes"
CREATE TABLE Clientes (
    id_cliente INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    telefono VARCHAR(20) NOT NULL,
    direccion VARCHAR(255) NOT NULL,
    PRIMARY KEY (id_cliente)
);

-- Creación de la tabla "Pedidos"
CREATE TABLE Pedidos (
    id_pedido INT NOT NULL AUTO_INCREMENT,
    id_cliente INT NOT NULL,
    fecha_pedido DATE NOT NULL,
    fecha_entrega DATE,
    estado VARCHAR(255) NOT NULL DEFAULT 'Pendiente',
    total DECIMAL(10, 2) NOT NULL,
    PRIMARY KEY (id_pedido),
    FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

-- Creación de la tabla "Productos_Pedidos"
CREATE TABLE Productos_Pedidos (
    id_pedido INT NOT NULL,
    id_producto INT NOT NULL,
    cantidad INT NOT NULL DEFAULT 1,
    precio_unitario DECIMAL(10, 2) NOT NULL,
    PRIMARY KEY (id_pedido, id_producto),
    FOREIGN KEY (id_pedido) REFERENCES Pedidos(id_pedido),
    FOREIGN KEY (id_producto) REFERENCES Productos(id_producto)
);

-- Insertar datos en la tabla "Productos"
INSERT INTO Productos (nombre, descripcion, precio, stock) VALUES
('Camiseta', 'Camiseta de algodón con diseño personalizado', 19.99, 50),
('Pantalón', 'Pantalón de mezclilla con bolsillos', 29.99, 40),
('Zapatos', 'Zapatos deportivos con cordones', 49.99, 30),
('Libro', 'Libro de ciencia ficción con tapa dura', 14.99, 25),
('Computadora', 'Computadora portátil con procesador Intel Core i5', 999.99, 10);

-- Insertar datos en la tabla "Categorias"
INSERT INTO Categorias (nombre, descripcion) VALUES
('Ropa', 'Prendas de vestir y calzado'),
('Tecnología', 'Gadgets y dispositivos electrónicos'),
('Libros', 'Libros de diversos géneros y autores'),
('Otros', 'Productos que no encajan en las categorías anteriores');

-- Insertar datos en la tabla "Productos_Categorias"
INSERT INTO Productos_Categorias (id_producto, id_categoria) VALUES
(1, 1),
(2, 1),
(3, 1),
(4, 3),
(5, 2);

-- Insertar datos en la tabla "Clientes"
INSERT INTO Clientes (nombre, apellido, email, telefono, direccion) VALUES
('Juan', 'García', 'juan.garcia@ejemplo.com', '123456789', 'Calle Principal 123'),
('María', 'Pérez', 'maria.perez@ejemplo.com', '987654321', 'Avenida del Sol 456'),
('Pedro', 'López', 'pedro.lopez@ejemplo.com', '234567890', 'Calle de la Luna 789');

-- Insertar datos en la tabla "Pedidos"
INSERT INTO Pedidos (id_cliente, fecha_pedido, fecha_entrega, estado, total) VALUES
(1, '2023-01-01', '2023-01-10', 'Completado', 129.97),
(2, '2023-02-01', '2023-02-15', 'En proceso', 649.96),
(3, '2023-03-01', '2023-03-20', 'Pendiente', 34.98);

-- Insertar datos en la tabla "Productos_Pedidos"
INSERT INTO Productos_Pedidos (id_pedido, id_producto, cantidad, precio_unitario) VALUES
(1, 1, 2, 19.99),
(1, 2, 1, 29.99),
(2, 5, 1, 999.99),
(3