```sql
-- Crear tabla de usuarios
CREATE TABLE usuarios (
  id_usuario INT NOT NULL AUTO_INCREMENT,
  nombre VARCHAR(255) NOT NULL,
  apellido VARCHAR(255) NOT NULL,
  email VARCHAR(255) NOT NULL UNIQUE,
  contraseña VARCHAR(255) NOT NULL,
  PRIMARY KEY (id_usuario)
);

-- Crear tabla de productos
CREATE TABLE productos (
  id_producto INT NOT NULL AUTO_INCREMENT,
  nombre VARCHAR(255) NOT NULL,
  precio DECIMAL(10, 2) NOT NULL,
  stock INT NOT NULL DEFAULT 0,
  PRIMARY KEY (id_producto)
);

-- Crear tabla de pedidos
CREATE TABLE pedidos (
  id_pedido INT NOT NULL AUTO_INCREMENT,
  id_usuario INT NOT NULL,
  fecha_pedido DATETIME NOT NULL,
  estado VARCHAR(255) NOT NULL DEFAULT 'Pendiente',
  total DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (id_pedido),
  FOREIGN KEY (id_usuario) REFERENCES usuarios(id_usuario)
);

-- Crear tabla de líneas de pedido
CREATE TABLE lineas_pedido (
  id_linea_pedido INT NOT NULL AUTO_INCREMENT,
  id_pedido INT NOT NULL,
  id_producto INT NOT NULL,
  cantidad INT NOT NULL,
  precio DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (id_linea_pedido),
  FOREIGN KEY (id_pedido) REFERENCES pedidos(id_pedido),
  FOREIGN KEY (id_producto) REFERENCES productos(id_producto)
);

-- Insertar datos de ejemplo en la tabla de usuarios
INSERT INTO usuarios (nombre, apellido, email, contraseña) VALUES
  ('Juan', 'García', 'juangarcia@ejemplo.com', '123456'),
  ('María', 'Pérez', 'mariaperez@ejemplo.com', '654321'),
  ('Pedro', 'López', 'pedrolopez@ejemplo.com', '987654');

-- Insertar datos de ejemplo en la tabla de productos
INSERT INTO productos (nombre, precio, stock) VALUES
  ('Camiseta roja', 19.99, 10),
  ('Pantalón azul', 29.99, 15),
  ('Zapatillas deportivas', 49.99, 20);

-- Insertar datos de ejemplo en la tabla de pedidos
INSERT INTO pedidos (id_usuario, fecha_pedido, estado, total) VALUES
  (1, '2023-03-08 18:30:00', 'Completado', 69.98),
  (2, '2023-03-10 12:00:00', 'Pendiente', 99.98),
  (3, '2023-03-12 15:30:00', 'Cancelado', 0.00);

-- Insertar datos de ejemplo en la tabla de líneas de pedido
INSERT INTO lineas_pedido (id_pedido, id_producto, cantidad, precio) VALUES
  (1, 1, 2, 39.98),
  (1, 2, 1, 29.99),
  (2, 1, 1, 19.99),
  (2, 3, 2, 99.98);

-- Select para consultar los datos de los usuarios que han realizado algún pedido
SELECT u.nombre, u.apellido, u.email
FROM usuarios u
JOIN pedidos p ON u.id_usuario = p.id_usuario
GROUP BY u.id_usuario;

-- Select para consultar los datos de los productos que se han vendido en algún pedido
SELECT p.nombre, SUM(lp.cantidad) AS unidades_vendidas
FROM productos p
JOIN lineas_pedido lp ON p.id_producto = lp.id_producto
GROUP BY p.id_producto;

-- Select para consultar el total de los pedidos realizados por cada usuario
SELECT u.nombre, u.apellido, SUM(p.total) AS total_pedidos
FROM usuarios u
JOIN pedidos p ON u.id_usuario = p.id_usuario
GROUP BY u.id_usuario;

-- Select para consultar los datos de los pedidos que han sido realizados por usuarios que han comprado más de un producto
SELECT p.id_pedido, p.fecha_pedido, p.estado, p.total, u.nombre, u.apellido
FROM pedidos p
JOIN usuarios u ON p.id_usuario = u.id_usuario
JOIN lineas_pedido lp ON p.id_pedido = lp.id_pedido
GROUP BY p.id_pedido
HAVING COUNT(DISTINCT lp.id_producto) > 1;
```

Este código crea una base de datos con cuatro tablas: usuarios, productos, pedidos y líneas de pedido.

La tabla de usuarios almacena los datos de los usuarios del sistema, como su nombre, apellido, email y contraseña.

La tabla de productos almacena los datos de los productos que se venden en el sistema, como su nombre, precio y stock.

La tabla de pedidos almacena los datos de los pedidos realizados por los usuarios, como la fecha del pedido, el estado del pedido y el total del pedido.

La tabla de líneas de pedido almacena los datos de las líneas de pedido, como el producto que se ha pedido, la cantidad pedida y el precio del producto.

El código también inserta datos de ejemplo en las cuatro tablas.

Finalmente, el código incluye cuatro consultas SQL que se pueden utilizar para obtener información de la base de datos.

La primera consulta devuelve los datos de los usuarios que han realizado algún pedido.

La segunda consulta devuelve los datos de los productos que se han vendido en algún pedido.

La tercera consulta devuelve el total de los pedidos realizados por cada usuario.

La cuarta consulta devuelve los datos de los pedidos que han sido realizados por usuarios que han comprado más de un producto.