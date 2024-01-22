```sql
-- Creación de base de datos
CREATE DATABASE base_de_datos_compleja;

-- Conexión a la base de datos
USE base_de_datos_compleja;

-- Creación de tablas
CREATE TABLE Clientes (
    id_cliente INT PRIMARY KEY AUTO_INCREMENT,
    nombre VARCHAR(50) NOT NULL,
    apellido VARCHAR(50) NOT NULL,
    correo_electronico VARCHAR(100) UNIQUE NOT NULL,
    telefono VARCHAR(20) NOT NULL,
    dirección VARCHAR(255) NOT NULL
);

CREATE TABLE Productos (
    id_producto INT PRIMARY KEY AUTO_INCREMENT,
    nombre VARCHAR(50) NOT NULL,
    precio DECIMAL(10, 2) NOT NULL,
    stock INT NOT NULL,
    descripcion TEXT
);

CREATE TABLE Pedidos (
    id_pedido INT PRIMARY KEY AUTO_INCREMENT,
    id_cliente INT NOT NULL,
    fecha_pedido DATE NOT NULL,
    fecha_entrega DATE,
    estado VARCHAR(20) NOT NULL,
    total DECIMAL(10, 2) NOT NULL,
    FOREIGN KEY (id_cliente) REFERENCES Clientes(id_cliente)
);

CREATE TABLE DetallePedidos (
    id_detalle_pedido INT PRIMARY KEY AUTO_INCREMENT,
    id_pedido INT NOT NULL,
    id_producto INT NOT NULL,
    cantidad INT NOT NULL,
    precio_unitario DECIMAL(10, 2) NOT NULL,
    FOREIGN KEY (id_pedido) REFERENCES Pedidos(id_pedido),
    FOREIGN KEY (id_producto) REFERENCES Productos(id_producto)
);

-- Inserción de datos en las tablas
INSERT INTO Clientes (nombre, apellido, correo_electronico, telefono, dirección) VALUES
    ('Juan', 'Pérez', 'juan.perez@email.com', '555-123-4567', 'Calle Principal 123'),
    ('María', 'García', 'maria.garcia@email.com', '555-234-5678', 'Avenida del Sol 456'),
    ('Pedro', 'López', 'pedro.lopez@email.com', '555-345-6789', 'Calle de la Luna 789');

INSERT INTO Productos (nombre, precio, stock, descripción) VALUES
    ('iPhone 14 Pro', 999.99, 10, 'El último y más avanzado teléfono inteligente de Apple'),
    ('Samsung Galaxy S23 Ultra', 899.99, 15, 'El teléfono inteligente más potente de Samsung'),
    ('Google Pixel 7 Pro', 799.99, 20, 'El mejor teléfono inteligente con cámara del mercado'),
    ('OnePlus 11', 699.99, 25, 'El teléfono inteligente más asequible con especificaciones de gama alta'),
    ('Xiaomi 13 Pro', 599.99, 30, 'El teléfono inteligente con la mejor relación calidad-precio');

INSERT INTO Pedidos (id_cliente, fecha_pedido, fecha_entrega, estado, total) VALUES
    (1, '2023-03-08', '2023-03-10', 'Completado', 999.99),
    (2, '2023-03-15', '2023-03-17', 'En proceso', 899.99),
    (3, '2023-03-22', NULL, 'Pendiente', 799.99);

INSERT INTO DetallePedidos (id_pedido, id_producto, cantidad, precio_unitario) VALUES
    (1, 1, 1, 999.99),
    (2, 2, 1, 899.99),
    (3, 3, 1, 799.99);

-- Consultas complejas
-- Obtener el nombre y el apellido de los clientes que han realizado al menos un pedido
SELECT Clientes.nombre, Clientes.apellido
FROM Clientes
INNER JOIN Pedidos ON Clientes.id_cliente = Pedidos.id_cliente
GROUP BY Clientes.id_cliente;

-- Obtener el nombre de los productos y el total de cada pedido
SELECT Productos.nombre AS producto, SUM(DetallePedidos.cantidad * DetallePedidos.precio_unitario) AS total_pedido
FROM DetallePedidos
INNER JOIN Productos ON DetallePedidos.id_producto = Productos.id_producto
INNER JOIN Pedidos ON DetallePedidos.id_pedido = Pedidos.id_pedido
GROUP BY producto;

-- Obtener el nombre de los clientes y el total de sus pedidos
SELECT Clientes.nombre AS cliente, SUM(DetallePedidos.cantidad * DetallePedidos.precio_unitario) AS total_cliente
FROM DetallePedidos
INNER JOIN Pedidos ON DetallePedidos.id_pedido = Pedidos.id_pedido
INNER JOIN Clientes ON Pedidos.id_cliente = Clientes.id_cliente
GROUP BY cliente;

-- Obtener el nombre de los productos más vendidos y la cantidad total vendida
SELECT Productos.nombre AS producto_mas_vendido, SUM(DetallePedidos.cantidad) AS cantidad_vendida
FROM DetallePedidos
INNER JOIN Productos ON DetallePedidos.id_producto = Productos.id_producto
GROUP BY producto_mas_vendido
ORDER BY cantidad_vendida DESC
LIMIT 3;

-- Obtener el nombre del cliente con el mayor número de pedidos
SELECT Clientes.nombre AS cliente_con_mas_pedidos, COUNT(Pedidos.id_pedido) AS numero_pedidos
FROM Clientes
INNER JOIN Pedidos ON Clientes.id_cliente = Pedidos.id_cliente
GROUP BY cliente_con_mas_pedidos
ORDER BY numero_pedidos DESC
LIMIT 1;
```

Explicación del código:

* La primera parte del código crea la base de datos y las tablas necesarias para almacenar los datos de los clientes, productos, pedidos y detalles de pedidos.
* La segunda parte del código inserta datos de ejemplo en las tablas.
* La tercera parte del código contiene consultas complejas que muestran cómo extraer información valiosa de los datos almacenados en las tablas.

Las consultas complejas incluyen:

* Obtener el nombre y el apellido de los clientes que han realizado al menos un pedido.
* Obtener el nombre de los productos y el total de cada pedido.
* Obtener el nombre de los clientes y el total de sus pedidos.
* Obtener el nombre de los productos más vendidos y la cantidad total vendida.
* Obtener el nombre del cliente con el mayor número de pedidos.

Este código muestra cómo utilizar SQL para almacenar, gestionar y consultar datos complejos.