```sql
-- Crear base de datos
CREATE DATABASE `mi_base_de_datos`;

-- Seleccionar base de datos
USE `mi_base_de_datos`;

-- Crear tabla de usuarios
CREATE TABLE `usuarios` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `nombre` VARCHAR(255) NOT NULL,
  `apellido` VARCHAR(255) NOT NULL,
  `email` VARCHAR(255) NOT NULL UNIQUE,
  `contrasena` VARCHAR(255) NOT NULL,
  `rol` ENUM('administrador', 'usuario') NOT NULL DEFAULT 'usuario',
  `fecha_creacion` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
);

-- Crear tabla de productos
CREATE TABLE `productos` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `nombre` VARCHAR(255) NOT NULL,
  `descripcion` TEXT NOT NULL,
  `precio` DECIMAL(10, 2) NOT NULL,
  `stock` INT NOT NULL DEFAULT 0,
  `fecha_creacion` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
);

-- Crear tabla de pedidos
CREATE TABLE `pedidos` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `usuario_id` INT NOT NULL,
  `fecha_pedido` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `estado` ENUM('pendiente', 'procesando', 'enviado', 'entregado', 'cancelado') NOT NULL DEFAULT 'pendiente',
  PRIMARY KEY (`id`),
  FOREIGN KEY (`usuario_id`) REFERENCES `usuarios` (`id`)
);

-- Crear tabla de productos_pedidos
CREATE TABLE `productos_pedidos` (
  `pedido_id` INT NOT NULL,
  `producto_id` INT NOT NULL,
  `cantidad` INT NOT NULL DEFAULT 1,
  PRIMARY KEY (`pedido_id`, `producto_id`),
  FOREIGN KEY (`pedido_id`) REFERENCES `pedidos` (`id`),
  FOREIGN KEY (`producto_id`) REFERENCES `productos` (`id`)
);

-- Insertar datos iniciales en la tabla de usuarios
INSERT INTO `usuarios` (`nombre`, `apellido`, `email`, `contrasena`, `rol`) VALUES
  ('Administrador', 'Apellidos', 'admin@example.com', '$2y$10$H1nj9/j913GWk7M5m/I.8eD1M7yvmcQ68WaZ58yM5SOhB/6NQ5V.', 'administrador'),
  ('Usuario', 'Apellidos', 'user@example.com', '$2y$10$H1nj9/j913GWk7M5m/I.8eD1M7yvmcQ68WaZ58yM5SOhB/6NQ5V.', 'usuario');

-- Insertar datos iniciales en la tabla de productos
INSERT INTO `productos` (`nombre`, `descripcion`, `precio`, `stock`) VALUES
  ('Producto 1', 'Descripción del producto 1', 100.00, 10),
  ('Producto 2', 'Descripción del producto 2', 200.00, 20),
  ('Producto 3', 'Descripción del producto 3', 300.00, 30);

-- Insertar datos iniciales en la tabla de pedidos
INSERT INTO `pedidos` (`usuario_id`, `fecha_pedido`, `estado`) VALUES
  (1, '2023-02-15 10:00:00', 'pendiente'),
  (2, '2023-02-16 12:00:00', 'procesando');

-- Insertar datos iniciales en la tabla de productos_pedidos
INSERT INTO `productos_pedidos` (`pedido_id`, `producto_id`, `cantidad`) VALUES
  (1, 1, 2),
  (1, 2, 1),
  (2, 3, 3);

-- Mostrar la estructura de la base de datos
SHOW DATABASES;
SHOW TABLES;
DESCRIBE usuarios;
DESCRIBE productos;
DESCRIBE pedidos;
DESCRIBE productos_pedidos;

-- Seleccionar datos de las tablas
SELECT * FROM usuarios;
SELECT * FROM productos;
SELECT * FROM pedidos;
SELECT * FROM productos_pedidos;

-- Filtrar datos de las tablas
SELECT * FROM usuarios WHERE rol = 'administrador';
SELECT * FROM productos WHERE precio > 200.00;
SELECT * FROM pedidos WHERE estado = 'pendiente';
SELECT * FROM productos_pedidos WHERE cantidad > 1;

-- Ordenar datos de las tablas
SELECT * FROM usuarios ORDER BY nombre ASC;
SELECT * FROM productos ORDER BY precio DESC;
SELECT * FROM pedidos ORDER BY fecha_pedido DESC;
SELECT * FROM productos_pedidos ORDER BY cantidad DESC;

-- Agrupar datos de las tablas
SELECT rol, COUNT(*) AS total_usuarios FROM usuarios GROUP BY rol;
SELECT nombre, SUM(cantidad) AS total_cantidad FROM productos_pedidos GROUP BY nombre;

-- Unir datos de las tablas
SELECT u.nombre, u.apellido, p.nombre AS producto_comprado, pp.cantidad
FROM usuarios u
JOIN pedidos p ON u.id = p.usuario_id
JOIN productos_pedidos pp ON p.id = pp.pedido_id;
```

Explicación del código:

- Se crea la base de datos `mi_base_de_datos`.
- Se selecciona la base de datos `mi_base_de_datos`.
- Se crean las tablas `usuarios`, `productos`, `pedidos` y `productos_pedidos`.
- Se insertan datos iniciales en las tablas.
- Se muestran la estructura de la base de datos y los datos de las tablas.
- Se filtran, ordenan, agrupan y unen los datos de las tablas.