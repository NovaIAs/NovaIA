```sql
-- Crear una función que calcule el área de un triángulo rectángulo.

CREATE FUNCTION area_triangulo_rectangulo(base NUMERIC, altura NUMERIC)
RETURNS NUMERIC
AS
$$
  -- Comprobar que los parámetros sean positivos.
  IF base <= 0 OR altura <= 0 THEN
    RAISE ERROR 'Los parámetros deben ser positivos.';
  END IF;

  -- Calcular el área del triángulo.
  RETURN 0.5 * base * altura;
$$
LANGUAGE SQL;

-- Crear una tabla para almacenar los datos de los clientes.

CREATE TABLE clientes (
  id SERIAL PRIMARY KEY,
  nombre VARCHAR(255) NOT NULL,
  apellido VARCHAR(255) NOT NULL,
  email VARCHAR(255) UNIQUE NOT NULL,
  telefono VARCHAR(20) UNIQUE NOT NULL,
  direccion VARCHAR(255) NOT NULL
);

-- Insertar algunos datos en la tabla de clientes.

INSERT INTO clientes (nombre, apellido, email, telefono, direccion) VALUES
  ('Juan', 'Pérez', 'juan.perez@example.com', '555-123-4567', 'Calle del Medio 123'),
  ('María', 'García', 'maria.garcia@example.com', '555-234-5678', 'Calle de la Luna 456'),
  ('Pedro', 'López', 'pedro.lopez@example.com', '555-345-6789', 'Calle del Sol 789');

-- Crear una tabla para almacenar los datos de los pedidos.

CREATE TABLE pedidos (
  id SERIAL PRIMARY KEY,
  cliente_id INTEGER NOT NULL,
  producto_id INTEGER NOT NULL,
  cantidad INTEGER NOT NULL,
  precio_unitario NUMERIC NOT NULL,
  total NUMERIC NOT NULL,
  fecha_pedido TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  estado VARCHAR(20) NOT NULL DEFAULT 'Pendiente'
);

-- Insertar algunos datos en la tabla de pedidos.

INSERT INTO pedidos (cliente_id, producto_id, cantidad, precio_unitario, total) VALUES
  (1, 1, 2, 10.00, 20.00),
  (1, 2, 1, 15.00, 15.00),
  (2, 1, 3, 10.00, 30.00),
  (2, 3, 2, 20.00, 40.00),
  (3, 2, 1, 15.00, 15.00),
  (3, 3, 3, 20.00, 60.00);

-- Crear una tabla para almacenar los datos de los productos.

CREATE TABLE productos (
  id SERIAL PRIMARY KEY,
  nombre VARCHAR(255) NOT NULL,
  descripcion VARCHAR(255),
  precio NUMERIC NOT NULL,
  stock INTEGER NOT NULL DEFAULT 0
);

-- Insertar algunos datos en la tabla de productos.

INSERT INTO productos (nombre, descripcion, precio, stock) VALUES
  ('Producto 1', 'Este es el producto 1.', 10.00, 10),
  ('Producto 2', 'Este es el producto 2.', 15.00, 20),
  ('Producto 3', 'Este es el producto 3.', 20.00, 30);

-- Crear una vista para mostrar los datos de los pedidos junto con los datos de los clientes y los productos.

CREATE VIEW pedidos_completos AS
SELECT
  p.id AS pedido_id,
  p.cliente_id,
  c.nombre AS cliente_nombre,
  c.apellido AS cliente_apellido,
  p.producto_id,
  prod.nombre AS producto_nombre,
  prod.descripcion AS producto_descripcion,
  p.cantidad,
  p.precio_unitario,
  p.total,
  p.fecha_pedido,
  p.estado
FROM pedidos p
JOIN clientes c ON p.cliente_id = c.id
JOIN productos prod ON p.producto_id = prod.id;

-- Crear un procedimiento almacenado para procesar un pedido.

CREATE PROCEDURE procesar_pedido(pedido_id INTEGER)
AS
$$
  -- Actualizar el estado del pedido a 'Procesado'.
  UPDATE pedidos
  SET estado = 'Procesado'
  WHERE id = pedido_id;

  -- Disminuir el stock de los productos del pedido.
  UPDATE productos
  SET stock = stock - p.cantidad
  FROM pedidos p
  WHERE p.id = pedido_id
  AND p.producto_id = productos.id;
$$
LANGUAGE SQL;

-- Crear un disparador para registrar los cambios en la tabla de pedidos.

CREATE TRIGGER pedidos_auditoria
AFTER INSERT OR UPDATE OR DELETE ON pedidos
FOR EACH ROW
EXECUTE PROCEDURE registrar_cambio();

-- Crear una tabla para almacenar los registros de auditoría.

CREATE TABLE pedidos_auditoria (
  id SERIAL PRIMARY KEY,
  fecha_cambio TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT NOW(),
  operacion VARCHAR(20) NOT NULL,
  pedido_id INTEGER NOT NULL,
  cliente_id INTEGER,
  producto_id INTEGER,
  cantidad INTEGER,
  precio_unitario NUMERIC,
  total NUMERIC,
  estado VARCHAR(20)
);

-- Crear una función para registrar los cambios en la tabla de pedidos.

CREATE FUNCTION registrar_cambio()
RETURNS TRIGGER AS
$$
  -- Insertar un registro en la tabla de auditoría.
  INSERT INTO pedidos_auditoria (fecha_cambio