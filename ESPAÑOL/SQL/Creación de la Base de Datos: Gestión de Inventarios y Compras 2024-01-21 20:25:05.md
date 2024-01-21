```sql
-- Este código crea una base de datos llamada "mi_base_de_datos" si no existe.

IF NOT EXISTS (SELECT * FROM sys.databases WHERE name = 'mi_base_de_datos')
BEGIN
    CREATE DATABASE mi_base_de_datos;
END;

-- Selecciona la base de datos "mi_base_de_datos".

USE mi_base_de_datos;

-- Crea una tabla llamada "clientes" con los siguientes campos:

CREATE TABLE clientes (
    id INT NOT NULL IDENTITY PRIMARY KEY,
    nombre VARCHAR(50) NOT NULL,
    apellido VARCHAR(50) NOT NULL,
    email VARCHAR(50) UNIQUE NOT NULL,
    telefono VARCHAR(20) UNIQUE NOT NULL
);

-- Crea una tabla llamada "productos" con los siguientes campos:

CREATE TABLE productos (
    id INT NOT NULL IDENTITY PRIMARY KEY,
    nombre VARCHAR(50) NOT NULL,
    descripcion VARCHAR(255),
    precio DECIMAL(10, 2) NOT NULL,
    stock INT NOT NULL
);

-- Crea una tabla llamada "pedidos" con los siguientes campos:

CREATE TABLE pedidos (
    id INT NOT NULL IDENTITY PRIMARY KEY,
    cliente_id INT NOT NULL,
    producto_id INT NOT NULL,
    cantidad INT NOT NULL,
    fecha_pedido DATETIME NOT NULL,
    fecha_entrega DATETIME,
    estado VARCHAR(20) NOT NULL
);

-- Crea una clave externa desde la tabla "pedidos" a la tabla "clientes".

ALTER TABLE pedidos ADD CONSTRAINT FK_cliente_id FOREIGN KEY (cliente_id) REFERENCES clientes(id);

-- Crea una clave externa desde la tabla "pedidos" a la tabla "productos".

ALTER TABLE pedidos ADD CONSTRAINT FK_producto_id FOREIGN KEY (producto_id) REFERENCES productos(id);

-- Inserta algunos datos en la tabla "clientes".

INSERT INTO clientes (nombre, apellido, email, telefono) VALUES
('Juan', 'Pérez', 'juan.perez@ejemplo.com', '555-123-4567'),
('María', 'García', 'maria.garcia@ejemplo.com', '555-234-5678'),
('Pedro', 'López', 'pedro.lopez@ejemplo.com', '555-345-6789');

-- Inserta algunos datos en la tabla "productos".

INSERT INTO productos (nombre, descripcion, precio, stock) VALUES
('iPhone 14', 'El último iPhone con las últimas funciones.', 1000.00, 10),
('Samsung Galaxy S23', 'El último Samsung Galaxy con las últimas funciones.', 900.00, 15),
('Google Pixel 7', 'El último Google Pixel con las últimas funciones.', 800.00, 20);

-- Inserta algunos datos en la tabla "pedidos".

INSERT INTO pedidos (cliente_id, producto_id, cantidad, fecha_pedido, fecha_entrega, estado) VALUES
(1, 1, 1, '2023-03-08', '2023-03-10', 'Entregado'),
(2, 2, 2, '2023-03-09', '2023-03-11', 'En camino'),
(3, 3, 3, '2023-03-10', '2023-03-12', 'Pendiente');

-- Selecciona todos los clientes y sus pedidos.

SELECT
    c.id AS cliente_id,
    c.nombre AS cliente_nombre,
    c.apellido AS cliente_apellido,
    c.email AS cliente_email,
    c.telefono AS cliente_telefono,
    p.id AS pedido_id,
    p.producto_id AS pedido_producto_id,
    p.cantidad AS pedido_cantidad,
    p.fecha_pedido AS pedido_fecha_pedido,
    p.fecha_entrega AS pedido_fecha_entrega,
    p.estado AS pedido_estado
FROM
    clientes c
INNER JOIN
    pedidos p ON c.id = p.cliente_id;

-- Selecciona todos los productos con un precio mayor a 900.

SELECT
    id,
    nombre,
    descripcion,
    precio,
    stock
FROM
    productos
WHERE
    precio > 900;

-- Actualiza el stock de un producto.

UPDATE
    productos
SET
    stock = stock - 1
WHERE
    id = 1;

-- Elimina un pedido.

DELETE FROM
    pedidos
WHERE
    id = 2;

-- Elimina un cliente y todos sus pedidos.

DELETE FROM
    clientes
WHERE
    id = 3;

-- Obtiene el número total de pedidos.

SELECT
    COUNT(*) AS total_pedidos
FROM
    pedidos;

-- Obtiene el producto más popular (el que más se ha vendido).

SELECT
    producto_id,
    nombre,
    COUNT(*) AS total_vendidos
FROM
    pedidos
GROUP BY
    producto_id, nombre
ORDER BY
    total_vendidos DESC
LIMIT 1;

-- Obtiene el cliente que más ha gastado.

SELECT
    c.id AS cliente_id,
    c.nombre AS cliente_nombre,
    c.apellido AS cliente_apellido,
    SUM(p.precio * p.cantidad) AS total_gastado
FROM
    clientes c
INNER JOIN
    pedidos p ON c.id = p.cliente_id
GROUP BY
    c.id, c.nombre, c.apellido
ORDER BY
    total_gastado DESC
LIMIT 1;
```

Explicación del código:

* El código comienza creando la base de datos "mi_base_de_datos" si no existe.
* A continuación, se selecciona la base de datos "mi_base_de_datos".
* Se crean las tablas "clientes", "productos" y "pedidos" con sus respectivos campos.
* Se crean claves externas desde la tabla "pedidos" a las tablas "clientes" y "productos".
* Se insertan algunos datos en las tablas "clientes", "productos" y "pedidos".
* Se realizan varias consultas para seleccionar, actualizar, eliminar y obtener información de las tablas.

Este código es un ejemplo de cómo crear una base de datos y realizar consultas complejas en SQL.