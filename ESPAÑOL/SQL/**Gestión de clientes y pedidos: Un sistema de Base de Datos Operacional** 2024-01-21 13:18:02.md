```sql
-- Crear una base de datos llamada "mibasededatos"
CREATE DATABASE mibasededatos;

-- Seleccionar la base de datos "mibasededatos"
USE mibasededatos;

-- Crear una tabla llamada "clientes" con los siguientes campos:
-- - id_cliente: campo entero autoincrement
-- - nombre: campo de texto con un máximo de 50 caracteres
-- - apellido: campo de texto con un máximo de 50 caracteres
-- - email: campo de texto con un máximo de 100 caracteres
-- - telefono: campo de texto con un máximo de 20 caracteres
CREATE TABLE clientes (
    id_cliente INT AUTO_INCREMENT PRIMARY KEY,
    nombre VARCHAR(50) NOT NULL,
    apellido VARCHAR(50) NOT NULL,
    email VARCHAR(100) UNIQUE NOT NULL,
    telefono VARCHAR(20) UNIQUE NOT NULL
);

-- Insertar datos en la tabla "clientes"
INSERT INTO clientes (nombre, apellido, email, telefono) VALUES
('Juan', 'García', 'juan.garcia@ejemplo.com', '912345678'),
('María', 'Pérez', 'maria.perez@ejemplo.com', '923456789'),
('Pedro', 'López', 'pedro.lopez@ejemplo.com', '934567890'),
('Ana', 'Fernández', 'ana.fernandez@ejemplo.com', '945678901'),
('Luis', 'Martínez', 'luis.martinez@ejemplo.com', '956789012');

-- Crear una tabla llamada "productos" con los siguientes campos:
-- - id_producto: campo entero autoincrement
-- - nombre: campo de texto con un máximo de 50 caracteres
-- - descripción: campo de texto con un máximo de 200 caracteres
-- - precio: campo decimal con una precisión de 10 y una escala de 2
CREATE TABLE productos (
    id_producto INT AUTO_INCREMENT PRIMARY KEY,
    nombre VARCHAR(50) NOT NULL,
    descripcion VARCHAR(200) NOT NULL,
    precio DECIMAL(10, 2) NOT NULL
);

-- Insertar datos en la tabla "productos"
INSERT INTO productos (nombre, descripcion, precio) VALUES
('Camiseta', 'Camiseta de algodón con cuello redondo y manga corta.', 10.99),
('Pantalón', 'Pantalón vaquero de corte ajustado.', 19.99),
('Zapatos', 'Zapatos deportivos de cuero.', 39.99),
('Libro', 'Libro de historia de la literatura española.', 24.99),
('Película', 'Película de comedia romántica.', 9.99);

-- Crear una tabla llamada "pedidos" con los siguientes campos:
-- - id_pedido: campo entero autoincrement
-- - id_cliente: campo entero que hace referencia a la tabla "clientes"
-- - id_producto: campo entero que hace referencia a la tabla "productos"
-- - cantidad: campo entero que indica la cantidad de productos pedidos
-- - fecha: campo de fecha que indica la fecha en que se realizó el pedido
CREATE TABLE pedidos (
    id_pedido INT AUTO_INCREMENT PRIMARY KEY,
    id_cliente INT NOT NULL,
    id_producto INT NOT NULL,
    cantidad INT NOT NULL,
    fecha DATE NOT NULL,
    FOREIGN KEY (id_cliente) REFERENCES clientes(id_cliente),
    FOREIGN KEY (id_producto) REFERENCES productos(id_producto)
);

-- Insertar datos en la tabla "pedidos"
INSERT INTO pedidos (id_cliente, id_producto, cantidad, fecha) VALUES
(1, 1, 2, '2023-02-13'),
(2, 2, 1, '2023-02-14'),
(3, 3, 3, '2023-02-15'),
(4, 4, 1, '2023-02-16'),
(5, 5, 2, '2023-02-17');

-- Crear una vista llamada "vw_pedidos_detalle" que muestre los siguientes campos:
-- - id_pedido: campo entero que indica el identificador del pedido
-- - nombre_cliente: campo de texto que indica el nombre del cliente
-- - apellido_cliente: campo de texto que indica el apellido del cliente
-- - nombre_producto: campo de texto que indica el nombre del producto
-- - descripcion_producto: campo de texto que indica la descripción del producto
-- - precio_producto: campo decimal que indica el precio del producto
-- - cantidad_pedida: campo entero que indica la cantidad de productos pedidos
-- - importe_total: campo decimal que indica el importe total del pedido
CREATE VIEW vw_pedidos_detalle AS
SELECT
    p.id_pedido,
    c.nombre AS nombre_cliente,
    c.apellido AS apellido_cliente,
    pr.nombre AS nombre_producto,
    pr.descripcion AS descripcion_producto,
    pr.precio AS precio_producto,
    pe.cantidad AS cantidad_pedida,
    pe.cantidad * pr.precio AS importe_total
FROM
    pedidos pe
JOIN
    clientes c ON pe.id_cliente = c.id_cliente
JOIN
    productos pr ON pe.id_producto = pr.id_producto;

-- Mostrar el importe total de cada pedido
SELECT
    p.id_pedido,
    c.nombre AS nombre_cliente,
    c.apellido AS apellido_cliente,
    SUM(pe.cantidad * pr.precio) AS importe_total
FROM
    pedidos pe
JOIN
    clientes c ON pe.id_cliente = c.id_cliente
JOIN
    productos pr ON pe.id_producto = pr.id_producto
GROUP BY
    p.id_pedido, c.nombre, c.apellido;

-- Crear una función escalar llamada "GetCustomerFullName" que devuelva el nombre completo de un cliente
CREATE FUNCTION GetCustomerFullName(@id_cliente INT) RETURNS VARCHAR(100) AS
BEGIN
    DECLARE @nombre VARCHAR(50);
    DECLARE @apellido VARCHAR(50);

    SELECT @nombre = nombre, @apellido = apellido FROM clientes WHERE id_cliente = @id_cliente;

    RETURN @nombre + ' ' + @apellido;
END;

-- Mostrar el nombre completo de cada cliente y el importe total de cada pedido
SELECT
    p.id_pedido,
    GetCustomerFullName(c.id_cliente) AS nombre_completo_cliente,
    SUM(pe.cantidad * pr.precio) AS importe_total
FROM
    pedidos pe
JOIN
    clientes c ON pe.id_cliente = c.id_cliente
JOIN
    productos pr ON pe.id_producto = pr.id_producto
GROUP BY
    p.id_pedido, GetCustomerFullName(c.id_cliente);

-- Crear un procedimiento almacenado llamado "AddCustomer" que inserte un nuevo cliente en la tabla "clientes"
CREATE PROCEDURE AddCustomer
(
    @nombre VARCHAR(50),
    @apellido VARCHAR(50),
    @email VARCHAR(100),
    @telefono VARCHAR(20)
)
AS
BEGIN
    INSERT INTO clientes (nombre, apellido, email, telefono) VALUES (@nombre, @apellido, @email, @telefono);
END;

-- Ejecutar el procedimiento almacenado "AddCustomer" para insertar un nuevo cliente
CALL AddCustomer('Juan', 'García', 'juan.garcia@ejemplo.com', '912345678');

-- Mostrar todos los clientes
SELECT * FROM clientes;
```

Explicación del código:

* Se crea una base de datos llamada "mibasededatos".
* Se selecciona la base de datos "mibasededatos" como base de datos actual.
* Se crean tres tablas: "clientes", "productos" y "pedidos".
* Se insertan datos en las tablas "clientes", "productos" y "pedidos".
* Se crea una vista llamada "vw_pedidos_detalle" que muestra información detallada sobre los pedidos.
* Se muestran el importe total de cada pedido y el nombre completo de cada cliente.
* Se crea una función escalar llamada "GetCustomerFullName" que devuelve el nombre completo de un cliente.
* Se muestra el nombre completo de cada cliente y el importe total de cada pedido.
* Se crea un procedimiento almacenado llamado "AddCustomer" que inserta un nuevo cliente en la tabla "clientes".
* Se ejecuta el procedimiento almacenado "AddCustomer" para insertar un nuevo cliente.
* Se muestran todos los clientes.