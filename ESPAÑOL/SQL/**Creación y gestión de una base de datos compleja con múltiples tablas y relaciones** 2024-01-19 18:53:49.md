```sql
-- Crear base de datos
CREATE DATABASE IF NOT EXISTS `bd_compleja`;

-- Seleccionar base de datos
USE `bd_compleja`;

-- Crear tablas

-- Tabla "usuarios"
CREATE TABLE `usuarios` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `nombre` VARCHAR(255) NOT NULL,
  `correo` VARCHAR(255) NOT NULL UNIQUE,
  `contraseña` VARCHAR(255) NOT NULL,
  `fecha_registro` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
);

-- Tabla "productos"
CREATE TABLE `productos` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `nombre` VARCHAR(255) NOT NULL,
  `descripcion` TEXT,
  `precio` DECIMAL(10, 2) NOT NULL,
  `stock` INT NOT NULL DEFAULT 0,
  `imagen` VARCHAR(255),
  `fecha_creacion` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`)
);

-- Tabla "pedidos"
CREATE TABLE `pedidos` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `usuario_id` INT NOT NULL,
  `fecha_pedido` TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `estado` ENUM('pendiente', 'procesando', 'enviado', 'entregado', 'cancelado') NOT NULL DEFAULT 'pendiente',
  `total` DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`usuario_id`) REFERENCES `usuarios`(`id`)
);

-- Tabla "pedido_detalles"
CREATE TABLE `pedido_detalles` (
  `id` INT NOT NULL AUTO_INCREMENT,
  `pedido_id` INT NOT NULL,
  `producto_id` INT NOT NULL,
  `cantidad` INT NOT NULL DEFAULT 1,
  `precio_unitario` DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (`id`),
  FOREIGN KEY (`pedido_id`) REFERENCES `pedidos`(`id`),
  FOREIGN KEY (`producto_id`) REFERENCES `productos`(`id`)
);

-- Insertar datos en la tabla "usuarios"
INSERT INTO `usuarios` (`nombre`, `correo`, `contraseña`, `fecha_registro`) VALUES
('Juan Pérez', 'juanperez@ejemplo.com', '123456', '2023-02-15 18:30:00'),
('María González', 'mariagonzalez@ejemplo.com', '654321', '2023-03-08 12:15:00'),
('Pedro Sánchez', 'pedrosanchez@ejemplo.com', '987654', '2023-04-12 15:45:00');

-- Insertar datos en la tabla "productos"
INSERT INTO `productos` (`nombre`, `descripcion`, `precio`, `stock`, `imagen`, `fecha_creacion`) VALUES
('iPhone 14 Pro', 'El último smartphone de Apple. Pantalla OLED de 6,1 pulgadas, cámara de 48 MP, chip A16 Bionic.', 1299.99, 10, 'iphone-14-pro.jpg', '2023-09-16 10:00:00'),
('Samsung Galaxy S23 Ultra', 'El teléfono Android más avanzado. Pantalla AMOLED de 6,8 pulgadas, cámara de 108 MP, chip Snapdragon 8 Gen 2.', 1199.99, 15, 'samsung-galaxy-s23-ultra.jpg', '2023-02-01 14:30:00'),
('Google Pixel 7 Pro', 'La mejor cámara en un teléfono. Pantalla LTPO AMOLED de 6,7 pulgadas, cámara de 50 MP, chip Tensor G2.', 899.99, 20, 'google-pixel-7-pro.jpg', '2022-10-18 17:00:00'),
('MacBook Air M2', 'La nueva MacBook Air con chip M2 de Apple. Pantalla Liquid Retina IPS de 13,6 pulgadas, 8 GB de RAM, 256 GB de almacenamiento.', 1099.99, 12, 'macbook-air-m2.jpg', '2022-06-24 11:15:00'),
('iPad Air 5', 'La nueva iPad Air con chip M1 de Apple. Pantalla Liquid Retina IPS de 10,9 pulgadas, 8 GB de RAM, 256 GB de almacenamiento.', 749.99, 18, 'ipad-air-5.jpg', '2022-03-08 13:45:00');

-- Insertar datos en la tabla "pedidos"
INSERT INTO `pedidos` (`usuario_id`, `fecha_pedido`, `estado`, `total`) VALUES
(1, '2023-09-20 17:30:00', 'procesando', 1499.98),
(2, '2023-10-05 12:00:00', 'enviado', 1949.97),
(3, '2023-11-10 10:15:00', 'entregado', 999.98);

-- Insertar datos en la tabla "pedido_detalles"
INSERT INTO `pedido_detalles` (`pedido_id`, `producto_id`, `cantidad`, `precio_unitario`) VALUES
(1, 1, 1, 1299.99),
(1, 2, 1, 1199.99),
(2, 3, 2, 899.99),
(3, 4, 1, 1099.99),
(3, 5, 1, 749.99);

-- Consultas

-- Obtener todos los productos que tengan un nombre que contenga "Apple".
SELECT * FROM productos WHERE nombre LIKE '%Apple%';

-- Obtener todos los pedidos realizados por un usuario específico, junto con los detalles de los productos pedidos.
SELECT * FROM pedidos p JOIN pedido_detalles pd ON p.id = pd.pedido_id JOIN productos pr ON pd.producto_id = pr.id WHERE p.usuario_id = 1;

-- Obtener los últimos 5 pedidos realizados, junto con el nombre del usuario que los realizó y el total del pedido.
SELECT * FROM pedidos p JOIN usuarios u ON p.usuario_id = u.id ORDER BY p.fecha_pedido DESC LIMIT 5;

-- Obtener el producto más popular, es decir, el producto que se ha vendido más veces.
SELECT producto_id, SUM(cantidad) AS cantidad_total FROM pedido_detalles GROUP BY producto_id ORDER BY cantidad_total DESC LIMIT 1;

-- Obtener el total de ventas de un producto específico.
SELECT SUM(pd.precio_unitario * pd.cantidad) AS total_ventas FROM pedido_detalles pd JOIN productos p ON pd.producto_id = p.id WHERE p.nombre = 'iPhone 14 Pro';

-- Obtener todos los usuarios que se hayan registrado en los últimos 30 días.
SELECT * FROM usuarios WHERE fecha_registro >= DATE_SUB(NOW(), INTERVAL 30 DAY);

-- Actualizar el stock de un producto específico.
UPDATE productos SET stock = stock - 1 WHERE nombre = 'iPhone 14 Pro';

-- Eliminar un producto específico.
DELETE FROM productos WHERE nombre = 'iPad Air 5';
```

Este código es complejo porque:

* Contiene varias tablas relacionadas entre sí mediante claves foráneas.
* Utiliza diferentes tipos de datos, incluyendo números enteros, decimales, fechas y cadenas de texto.
* Realiza varias consultas complejas, incluyendo joins, agrupaciones y subconsultas.
* Utiliza funciones de fecha y hora para filtrar los datos.
* Actualiza y elimina datos.