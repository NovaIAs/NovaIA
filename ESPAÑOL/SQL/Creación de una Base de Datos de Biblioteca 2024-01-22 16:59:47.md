```sql
-- Crear una base de datos llamada "biblioteca"
CREATE DATABASE biblioteca;

-- Seleccionar la base de datos "biblioteca"
USE biblioteca;

-- Crear una tabla llamada "autores"
CREATE TABLE autores (
    id_autor INT AUTO_INCREMENT PRIMARY KEY,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255) NOT NULL,
    nacionalidad VARCHAR(255) NOT NULL
);

-- Crear una tabla llamada "libros"
CREATE TABLE libros (
    id_libro INT AUTO_INCREMENT PRIMARY KEY,
    titulo VARCHAR(255) NOT NULL,
    autor INT NOT NULL,
    fecha_publicacion DATE,
    precio DECIMAL(10, 2) NOT NULL,

    -- Establecer la relación entre la tabla "libros" y la tabla "autores"
    CONSTRAINT FOREIGN KEY (autor) REFERENCES autores(id_autor)
);

-- Crear una tabla llamada "ventas"
CREATE TABLE ventas (
    id_venta INT AUTO_INCREMENT PRIMARY KEY,
    libro INT NOT NULL,
    fecha_venta DATE,
    cantidad INT NOT NULL,

    -- Establecer la relación entre la tabla "ventas" y la tabla "libros"
    CONSTRAINT FOREIGN KEY (libro) REFERENCES libros(id_libro)
);

-- Crear una tabla llamada "clientes"
CREATE TABLE clientes (
    id_cliente INT AUTO_INCREMENT PRIMARY KEY,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255) NOT NULL,
    dirección VARCHAR(255) NOT NULL,
    teléfono VARCHAR(255) NOT NULL
);

-- Crear una tabla llamada "pedidos"
CREATE TABLE pedidos (
    id_pedido INT AUTO_INCREMENT PRIMARY KEY,
    cliente INT NOT NULL,
    fecha_pedido DATE,
    total DECIMAL(10, 2) NOT NULL,
    estado VARCHAR(255) NOT NULL,

    -- Establecer la relación entre la tabla "pedidos" y la tabla "clientes"
    CONSTRAINT FOREIGN KEY (cliente) REFERENCES clientes(id_cliente)
);

-- Crear una tabla llamada "detalles_pedido"
CREATE TABLE detalles_pedido (
    id_detalle INT AUTO_INCREMENT PRIMARY KEY,
    pedido INT NOT NULL,
    libro INT NOT NULL,
    cantidad INT NOT NULL,
    precio DECIMAL(10, 2) NOT NULL,

    -- Establecer las relaciones entre la tabla "detalles_pedido" y las tablas "pedidos" y "libros"
    CONSTRAINT FOREIGN KEY (pedido) REFERENCES pedidos(id_pedido),
    CONSTRAINT FOREIGN KEY (libro) REFERENCES libros(id_libro)
);

-- Insertar datos en la tabla "autores"
INSERT INTO autores (nombre, apellido, nacionalidad) VALUES
    ('Juan', 'Pérez', 'España'),
    ('María', 'García', 'México'),
    ('Pedro', 'López', 'Argentina'),
    ('Ana', 'Fernández', 'Colombia'),
    ('José', 'Martínez', 'Venezuela');

-- Insertar datos en la tabla "libros"
INSERT INTO libros (titulo, autor, fecha_publicacion, precio) VALUES
    ('El Quijote de la Mancha', 1, '1605-01-16', 20.00),
    ('Cien años de soledad', 2, '1967-06-05', 15.00),
    ('El Señor de los Anillos', 3, '1954-07-29', 30.00),
    ('Harry Potter y la piedra filosofal', 4, '1997-06-26', 12.00),
    ('El código Da Vinci', 5, '2003-03-12', 25.00);

-- Insertar datos en la tabla "ventas"
INSERT INTO ventas (libro, fecha_venta, cantidad) VALUES
    (1, '2023-02-13', 5),
    (2, '2023-03-08', 3),
    (3, '2023-04-15', 2),
    (4, '2023-05-22', 4),
    (5, '2023-06-29', 1);

-- Insertar datos en la tabla "clientes"
INSERT INTO clientes (nombre, apellido, dirección, teléfono) VALUES
    ('Carlos', 'Ruiz', 'Calle 10 # 20-30', '555-1234'),
    ('Sara', 'Ortiz', 'Calle 15 # 30-40', '555-2345'),
    ('Luis', 'Díaz', 'Calle 20 # 40-50', '555-3456'),
    ('Rosa', 'Morales', 'Calle 25 # 50-60', '555-4567'),
    ('David', 'Hernández', 'Calle 30 # 60-70', '555-5678');

-- Insertar datos en la tabla "pedidos"
INSERT INTO pedidos (cliente, fecha_pedido, total, estado) VALUES
    (1, '2023-07-06', 100.00, 'Procesado'),
    (2, '2023-08-13', 75.00, 'Enviado'),
    (3, '2023-09-20', 120.00, 'Entregado'),
    (4, '2023-10-27', 48.00, 'Procesado'),
    (5, '2023-11-24', 25.00, 'Enviado');

-- Insertar datos en la tabla "detalles_pedido"
INSERT INTO detalles_pedido (pedido, libro, cantidad, precio) VALUES
    (1, 1, 2, 40.00),
    (1, 2, 1, 15.00),
    (2, 3, 2, 60.00),
    (3, 4, 4, 48.00),
    (4, 5, 1, 25.00);

EXPLICACIÓN:

- Creamos varias tablas: "autores", "libros", "ventas", "clientes", "pedidos" y "detalles_pedido". Cada tabla tiene sus propias columnas y restricciones.

- Insertamos datos en cada tabla utilizando la instrucción INSERT INTO. Los datos incluyen información sobre autores, libros, ventas, clientes, pedidos y detalles de pedidos.

- Utilizamos las relaciones entre las tablas mediante las claves foráneas para garantizar la integridad de los datos y la consistencia de la base de datos.

- Este código crea una base de datos completa con información sobre autores, libros, ventas, clientes y pedidos. Esta base de datos puede utilizarse para realizar consultas y análisis de datos relacionados con la gestión de una biblioteca, una librería o una tienda online de libros.