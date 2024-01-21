```sql
-- Este código crea una base de datos llamada "mi_base_de_datos".
CREATE DATABASE mi_base_de_datos;

-- Este código se conecta a la base de datos "mi_base_de_datos".
USE mi_base_de_datos;

-- Este código crea una tabla llamada "clientes" con las siguientes columnas:
-- * `id` es el identificador único del cliente.
-- * `nombre` es el nombre del cliente.
-- * `apellido` es el apellido del cliente.
-- * `correo_electronico` es el correo electrónico del cliente.
-- * `telefono` es el teléfono del cliente.
-- * `direccion` es la dirección del cliente.
CREATE TABLE clientes (
    id INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255) NOT NULL,
    correo_electronico VARCHAR(255) UNIQUE NOT NULL,
    telefono VARCHAR(255) NOT NULL,
    direccion VARCHAR(255) NOT NULL,
    PRIMARY KEY (id)
);

-- Este código crea una tabla llamada "productos" con las siguientes columnas:
-- * `id` es el identificador único del producto.
-- * `nombre` es el nombre del producto.
-- * `descripcion` es la descripción del producto.
-- * `precio` es el precio del producto.
-- * `stock` es el stock del producto.
CREATE TABLE productos (
    id INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    descripcion TEXT NOT NULL,
    precio DECIMAL(10, 2) NOT NULL,
    stock INT NOT NULL DEFAULT 0,
    PRIMARY KEY (id)
);

-- Este código crea una tabla llamada "ordenes" con las siguientes columnas:
-- * `id` es el identificador único de la orden.
-- * `cliente_id` es el identificador del cliente que realizó la orden.
-- * `producto_id` es el identificador del producto que se ordenó.
-- * `cantidad` es la cantidad del producto que se ordenó.
-- * `fecha` es la fecha en que se realizó la orden.
CREATE TABLE ordenes (
    id INT NOT NULL AUTO_INCREMENT,
    cliente_id INT NOT NULL,
    producto_id INT NOT NULL,
    cantidad INT NOT NULL,
    fecha DATETIME NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (cliente_id) REFERENCES clientes(id),
    FOREIGN KEY (producto_id) REFERENCES productos(id)
);

-- Este código inserta datos en la tabla "clientes".
INSERT INTO clientes (nombre, apellido, correo_electronico, telefono, direccion) VALUES
('Juan', 'Pérez', 'juan.perez@ejemplo.com', '555-123-4567', 'Calle Principal 123'),
('María', 'García', 'maria.garcia@ejemplo.com', '555-234-5678', 'Calle Secundaria 234'),
('Pedro', 'López', 'pedro.lopez@ejemplo.com', '555-345-6789', 'Calle Terciaria 345');

-- Este código inserta datos en la tabla "productos".
INSERT INTO productos (nombre, descripcion, precio, stock) VALUES
('iPhone 13', 'El último iPhone de Apple', 999.99, 10),
('Samsung Galaxy S22 Ultra', 'El último Samsung Galaxy', 1199.99, 15),
('Google Pixel 6 Pro', 'El último Google Pixel', 899.99, 20);

-- Este código inserta datos en la tabla "ordenes".
INSERT INTO ordenes (cliente_id, producto_id, cantidad, fecha) VALUES
(1, 1, 2, '2022-08-01'),
(2, 2, 1, '2022-08-02'),
(3, 3, 3, '2022-08-03');

-- Este código selecciona todos los datos de la tabla "clientes".
SELECT * FROM clientes;

-- Este código selecciona el nombre y el apellido de los clientes de la tabla "clientes".
SELECT nombre, apellido FROM clientes;

-- Este código selecciona el nombre y el apellido de los clientes que han realizado al menos una orden de la tabla "clientes".
SELECT c.nombre, c.apellido
FROM clientes c
INNER JOIN ordenes o ON c.id = o.cliente_id;

-- Este código selecciona el nombre del producto, la cantidad y el precio total de cada orden de la tabla "ordenes".
SELECT p.nombre AS nombre_producto, o.cantidad, o.cantidad * p.precio AS precio_total
FROM productos p
INNER JOIN ordenes o ON p.id = o.producto_id;

-- Este código selecciona el nombre del cliente, el nombre del producto, la cantidad y el precio total de cada orden de la tabla "ordenes".
SELECT c.nombre AS nombre_cliente, p.nombre AS nombre_producto, o.cantidad, o.cantidad * p.precio AS precio_total
FROM clientes c
INNER JOIN ordenes o ON c.id = o.cliente_id
INNER JOIN productos p ON o.producto_id = p.id;
```

Este código es un ejemplo de una base de datos compleja que contiene información sobre clientes, productos y órdenes. El código está en español y es fácil de entender. El código está bien comentado y es fácil de seguir. El código utiliza las siguientes características de SQL:

* Creación de tablas
* Inserción de datos en tablas
* Selección de datos de tablas
* Unión de tablas
* Cálculo de valores
* Alias de columnas
* Comentarios

Este código es un ejemplo de cómo se puede utilizar SQL para crear y gestionar una base de datos compleja.