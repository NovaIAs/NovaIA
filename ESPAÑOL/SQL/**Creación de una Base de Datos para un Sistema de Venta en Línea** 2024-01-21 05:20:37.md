```sql
-- Crear una tabla "Usuarios" con columnas "id", "nombre", "apellido", "correo_electronico" y "contraseña".
CREATE TABLE Usuarios (
    id INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255) NOT NULL,
    correo_electronico VARCHAR(255) UNIQUE NOT NULL,
    contraseña VARCHAR(255) NOT NULL,
    PRIMARY KEY (id)
);

-- Crear una tabla "Productos" con columnas "id", "nombre", "descripcion", "precio" y "stock".
CREATE TABLE Productos (
    id INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    descripcion TEXT,
    precio DECIMAL(10, 2) NOT NULL,
    stock INT NOT NULL DEFAULT 0,
    PRIMARY KEY (id)
);

-- Crear una tabla "Pedidos" con columnas "id", "usuario_id", "fecha_pedido", "total" y "estado".
CREATE TABLE Pedidos (
    id INT NOT NULL AUTO_INCREMENT,
    usuario_id INT NOT NULL,
    fecha_pedido DATETIME NOT NULL,
    total DECIMAL(10, 2) NOT NULL,
    estado VARCHAR(255) NOT NULL DEFAULT 'Pendiente',
    PRIMARY KEY (id),
    FOREIGN KEY (usuario_id) REFERENCES Usuarios(id)
);

-- Crear una tabla "Productos_pedidos" con columnas "id", "producto_id", "pedido_id", "cantidad" y "precio_unitario".
CREATE TABLE Productos_pedidos (
    id INT NOT NULL AUTO_INCREMENT,
    producto_id INT NOT NULL,
    pedido_id INT NOT NULL,
    cantidad INT NOT NULL,
    precio_unitario DECIMAL(10, 2) NOT NULL,
    PRIMARY KEY (id),
    FOREIGN KEY (producto_id) REFERENCES Productos(id),
    FOREIGN KEY (pedido_id) REFERENCES Pedidos(id)
);

-- Insertar datos en la tabla "Usuarios".
INSERT INTO Usuarios (nombre, apellido, correo_electronico, contraseña) VALUES
    ('Juan', 'Pérez', 'juan.perez@ejemplo.com', '123456'),
    ('María', 'García', 'maria.garcia@ejemplo.com', '654321'),
    ('Pedro', 'López', 'pedro.lopez@ejemplo.com', '987654');

-- Insertar datos en la tabla "Productos".
INSERT INTO Productos (nombre, descripcion, precio, stock) VALUES
    ('iPhone 13 Pro', 'El último y mejor iPhone de Apple', 999.99, 10),
    ('Samsung Galaxy S22 Ultra', 'El teléfono Android más avanzado', 1199.99, 15),
    ('Google Pixel 6 Pro', 'El mejor teléfono con cámara de Google', 899.99, 20);

-- Insertar datos en la tabla "Pedidos".
INSERT INTO Pedidos (usuario_id, fecha_pedido, total, estado) VALUES
    (1, '2023-02-13 18:30:00', 999.99, 'Completado'),
    (2, '2023-03-08 12:15:00', 1199.99, 'Pendiente'),
    (3, '2023-04-12 10:30:00', 899.99, 'Cancelado');

-- Insertar datos en la tabla "Productos_pedidos".
INSERT INTO Productos_pedidos (producto_id, pedido_id, cantidad, precio_unitario) VALUES
    (1, 1, 1, 999.99),
    (2, 2, 1, 1199.99),
    (3, 3, 1, 899.99);

-- Consulta para obtener todos los pedidos de un usuario específico.
SELECT *
FROM Pedidos
WHERE usuario_id = 1;

-- Consulta para obtener todos los productos de un pedido específico.
SELECT *
FROM Productos_pedidos
WHERE pedido_id = 1;

-- Consulta para obtener el total de todos los pedidos de un usuario específico.
SELECT SUM(total)
FROM Pedidos
WHERE usuario_id = 1;

-- Consulta para obtener el producto más popular (el que se ha pedido más veces) en un rango de fechas específico.
SELECT producto_id, SUM(cantidad) AS cantidad_total
FROM Productos_pedidos
WHERE pedido_id IN (
    SELECT id
    FROM Pedidos
    WHERE fecha_pedido BETWEEN '2023-01-01' AND '2023-12-31'
)
GROUP BY producto_id
ORDER BY cantidad_total DESC
LIMIT 1;

-- Consulta para obtener el usuario que ha gastado más dinero en pedidos en un rango de fechas específico.
SELECT usuario_id, SUM(total) AS total_gastado
FROM Pedidos
WHERE fecha_pedido BETWEEN '2023-01-01' AND '2023-12-31'
GROUP BY usuario_id
ORDER BY total_gastado DESC
LIMIT 1;
```

Explicación del código:

* El código crea cuatro tablas: "Usuarios", "Productos", "Pedidos" y "Productos_pedidos". Cada tabla tiene sus propias columnas y restricciones.
* El código inserta datos en las tablas "Usuarios", "Productos", "Pedidos" y "Productos_pedidos" utilizando la instrucción `INSERT INTO`.
* El código contiene varias consultas SQL que se utilizan para obtener datos de las tablas. Cada consulta está comentada para explicar su propósito.