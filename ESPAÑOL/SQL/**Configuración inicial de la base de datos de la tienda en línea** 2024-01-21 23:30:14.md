```sql
-- Crear esquema de base de datos
CREATE SCHEMA IF NOT EXISTS mi_esquema;
USE mi_esquema;

-- Crear tabla `productos`
CREATE TABLE productos (
  id_producto INT NOT NULL AUTO_INCREMENT,
  nombre VARCHAR(255) NOT NULL,
  descripcion TEXT,
  precio DECIMAL(10, 2) NOT NULL,
  existencias INT NOT NULL DEFAULT 0,
  id_categoria INT NOT NULL,
  PRIMARY KEY (id_producto),
  FOREIGN KEY (id_categoria) REFERENCES categorias(id_categoria)
);

-- Crear tabla `categorias`
CREATE TABLE categorias (
  id_categoria INT NOT NULL AUTO_INCREMENT,
  nombre VARCHAR(255) NOT NULL,
  descripcion TEXT,
  PRIMARY KEY (id_categoria)
);

-- Insertar datos en la tabla `categorias`
INSERT INTO categorias (nombre, descripcion) VALUES
  ('Ropa', 'Ropa para hombres, mujeres y niños'),
  ('Electrónica', 'Electrónica de consumo, informática y telefonía'),
  ('Hogar', 'Muebles, decoración y artículos para el hogar'),
  ('Comida y Bebida', 'Alimentación, bebidas y snacks'),
  ('Juguetes', 'Juguetes y juegos para niños de todas las edades');

-- Insertar datos en la tabla `productos`
INSERT INTO productos (nombre, descripcion, precio, existencias, id_categoria) VALUES
  ('Camiseta', 'Camiseta de algodón básica', 9.99, 100, 1),
  ('Jeans', 'Jeans ajustados de mezclilla', 19.99, 50, 1),
  ('Zapatillas', 'Zapatillas deportivas cómodas', 29.99, 30, 1),
  ('Portátil', 'Portátil con procesador Intel Core i5', 699.99, 20, 2),
  ('Teléfono móvil', 'Teléfono móvil con pantalla táctil de 6,5"', 299.99, 40, 2),
  ('Televisor', 'Televisor LED de 43"', 399.99, 15, 2),
  ('Sofá', 'Sofá de tres plazas tapizado en tela', 499.99, 10, 3),
  ('Cama', 'Cama de matrimonio con colchón incluido', 999.99, 5, 3),
  ('Mesa', 'Mesa de comedor de madera extensible', 299.99, 8, 3),
  ('Leche', 'Leche entera de vaca', 1.99, 100, 4),
  ('Pan', 'Pan de molde integral', 2.49, 50, 4),
  ('Queso', 'Queso cheddar en lonchas', 3.99, 30, 4),
  ('Juguete', 'Juguete educativo para niños pequeños', 14.99, 20, 5),
  ('Juego de mesa', 'Juego de mesa familiar para todas las edades', 24.99, 10, 5);

-- Crear vista `productos_en_stock` que muestre los productos con existencias disponibles
CREATE VIEW productos_en_stock AS
  SELECT id_producto, nombre, precio, existencias
  FROM productos
  WHERE existencias > 0;

-- Crear función `calcular_total_compra(id_cliente, id_producto, cantidad)` que calcule el total de una compra
CREATE FUNCTION calcular_total_compra(id_cliente INT, id_producto INT, cantidad INT) RETURNS DECIMAL(10, 2)
BEGIN
  DECLARE precio_unitario DECIMAL(10, 2);

  -- Obtener el precio unitario del producto
  SELECT precio INTO precio_unitario FROM productos WHERE id_producto = id_producto;

  -- Calcular el total de la compra
  RETURN precio_unitario * cantidad;
END;

-- Crear procedimiento almacenado `realizar_compra(id_cliente, id_producto, cantidad)` que registre una compra
CREATE PROCEDURE realizar_compra(id_cliente INT, id_producto INT, cantidad INT)
BEGIN
  -- Actualizar las existencias del producto
  UPDATE productos SET existencias = existencias - cantidad WHERE id_producto = id_producto;

  -- Registrar la compra en la tabla `compras`
  INSERT INTO compras (id_cliente, id_producto, cantidad, fecha_compra)
    VALUES (id_cliente, id_producto, cantidad, CURRENT_TIMESTAMP);
END;

-- Crear disparador `actualizar_stock_productos` que actualice las existencias de los productos al modificar su precio
CREATE TRIGGER actualizar_stock_productos
ON productos
FOR UPDATE
AS
BEGIN
  -- Actualizar las existencias del producto con un 10% de descuento
  UPDATE productos SET existencias = existencias * 0.9 WHERE id_producto = OLD.id_producto;
END;

-- Crear índice en la columna `nombre` de la tabla `productos`
CREATE INDEX idx_productos_nombre ON productos (nombre);

-- Crear índice en la columna `id_categoria` de la tabla `productos`
CREATE INDEX idx_productos_id_categoria ON productos (id_categoria);

-- Crear índice en la columna `id_cliente` de la tabla `compras`
CREATE INDEX idx_compras_id_cliente ON compras (id_cliente);

-- Crear índice en la columna `id_producto` de la tabla `compras`
CREATE INDEX idx_compras_id_producto ON compras (id_producto);
```

Explicación del código:

* Se crea un esquema llamado "mi_esquema" para organizar las tablas y objetos de la base de datos.
* Se crean dos tablas: "productos" y "categorias". La tabla "productos" almacena información sobre los productos disponibles en la tienda, mientras que la tabla "categorias" almacena las categorías de productos.
* Se insertan datos en las tablas "productos" y "categorias" utilizando la sentencia `INSERT INTO`.
* Se crea una vista llamada "productos_en_stock" que muestra solo los productos que tienen existencias disponibles.
* Se crea una función llamada "calcular_total_compra" que calcula el total de una compra dada la identificación del cliente, la identificación del producto y la cantidad comprada.
* Se crea un procedimiento almacenado llamado "realizar_compra" que registra una compra en la base de datos y actualiza las existencias del producto comprado.
* Se crea un disparador llamado "actualizar_stock_productos" que actualiza las existencias de los productos cuando se modifica su precio.
* Se crean índices en las columnas clave de las tablas para mejorar el rendimiento de las consultas.