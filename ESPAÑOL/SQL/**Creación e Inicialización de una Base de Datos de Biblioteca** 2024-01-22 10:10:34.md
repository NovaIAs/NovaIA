```sql
-- Creación de la base de datos "biblioteca"
CREATE DATABASE biblioteca;

-- Selección de la base de datos "biblioteca"
USE biblioteca;

-- Creación de la tabla "libros"
CREATE TABLE libros (
    id_libro INT NOT NULL AUTO_INCREMENT,
    titulo VARCHAR(255) NOT NULL,
    autor VARCHAR(255) NOT NULL,
    genero VARCHAR(255),
    anio_publicacion INT,
    precio DECIMAL(10,2),
    PRIMARY KEY (id_libro)
);

-- Creación de la tabla "autores"
CREATE TABLE autores (
    id_autor INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255),
    nacionalidad VARCHAR(255),
    fecha_nacimiento DATE,
    PRIMARY KEY (id_autor)
);

-- Creación de la tabla "generos"
CREATE TABLE generos (
    id_genero INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    descripcion VARCHAR(255),
    PRIMARY KEY (id_genero)
);

-- Creación de la tabla "prestamos"
CREATE TABLE prestamos (
    id_prestamo INT NOT NULL AUTO_INCREMENT,
    id_libro INT NOT NULL,
    id_usuario INT NOT NULL,
    fecha_prestamo DATE,
    fecha_devolucion DATE,
    PRIMARY KEY (id_prestamo),
    FOREIGN KEY (id_libro) REFERENCES libros(id_libro),
    FOREIGN KEY (id_usuario) REFERENCES usuarios(id_usuario)
);

-- Creación de la tabla "usuarios"
CREATE TABLE usuarios (
    id_usuario INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255),
    correo_electronico VARCHAR(255) UNIQUE,
    contraseña VARCHAR(255),
    PRIMARY KEY (id_usuario)
);

-- Inserción de datos en la tabla "autores"
INSERT INTO autores (nombre, apellido, nacionalidad, fecha_nacimiento) VALUES
    ('Gabriel', 'García Márquez', 'Colombia', '1927-03-06'),
    ('Mario', 'Vargas Llosa', 'Perú', '1936-03-28'),
    ('Isabel', 'Allende', 'Chile', '1942-08-02'),
    ('Paulo', 'Coelho', 'Brasil', '1947-08-24'),
    ('J.K.', 'Rowling', 'Reino Unido', '1965-07-31');

-- Inserción de datos en la tabla "generos"
INSERT INTO generos (nombre, descripcion) VALUES
    ('Novela', 'Género literario que narra una historia ficticia.'),
    ('Poesía', 'Género literario que utiliza un lenguaje figurativo y musical.'),
    ('Teatro', 'Género literario que se representa en un escenario.'),
    ('Ensayo', 'Género literario que expone y analiza un tema específico.'),
    ('Historia', 'Género literario que narra acontecimientos pasados.');

-- Inserción de datos en la tabla "libros"
INSERT INTO libros (titulo, autor, genero, anio_publicacion, precio) VALUES
    ('Cien años de soledad', 'Gabriel García Márquez', 'Novela', 1967, 20.00),
    ('La casa de los espíritus', 'Isabel Allende', 'Novela', 1982, 15.00),
    ('El alquimista', 'Paulo Coelho', 'Novela', 1988, 12.00),
    ('Harry Potter y la piedra filosofal', 'J.K. Rowling', 'Novela', 1997, 10.00),
    ('El ingenioso hidalgo Don Quijote de la Mancha', 'Miguel de Cervantes Saavedra', 'Novela', 1605, 18.00);

-- Inserción de datos en la tabla "usuarios"
INSERT INTO usuarios (nombre, apellido, correo_electronico, contraseña) VALUES
    ('Juan', 'Pérez', 'juan.perez@ejemplo.com', '123456'),
    ('María', 'López', 'maria.lopez@ejemplo.com', '654321'),
    ('Pedro', 'García', 'pedro.garcia@ejemplo.com', '789012'),
    ('Ana', 'Fernández', 'ana.fernandez@ejemplo.com', '456789'),
    ('Luis', 'Romero', 'luis.romero@ejemplo.com', '987654');

-- Inserción de datos en la tabla "prestamos"
INSERT INTO prestamos (id_libro, id_usuario, fecha_prestamo, fecha_devolucion) VALUES
    (1, 1, '2023-02-15', '2023-03-15'),
    (2, 2, '2023-03-01', '2023-03-31'),
    (3, 3, '2023-04-01', '2023-04-30'),
    (4, 4, '2023-05-01', '2023-05-31'),
    (5, 5, '2023-06-01', '2023-06-30');

-- Consulta para obtener los libros más prestados
SELECT libros.titulo, COUNT(prestamos.id_libro) AS prestamos_totales
FROM libros
JOIN prestamos ON libros.id_libro = prestamos.id_libro
GROUP BY libros.titulo
ORDER BY prestamos_totales DESC
LIMIT 5;

-- Consulta para obtener los autores más populares
SELECT autores.nombre, autores.apellido, COUNT(libros.id_autor) AS libros_publicados
FROM autores
JOIN libros ON autores.id_autor = libros.id_autor
GROUP BY autores.nombre, autores.apellido
ORDER BY libros_publicados DESC
LIMIT 5;

-- Consulta para obtener los géneros más populares
SELECT generos.nombre, COUNT(libros.id_genero) AS libros_publicados
FROM generos
JOIN libros ON generos.id_genero = libros.id_genero
GROUP BY generos.nombre
ORDER BY libros_publicados DESC
LIMIT 5;
```

Explicación del código:

1. Se crea la base de datos "biblioteca" y se selecciona.
2. Se crean las tablas "libros", "autores", "generos", "prestamos" y "usuarios" con sus respectivas columnas y restricciones.
3. Se insertan datos en las tablas "autores", "generos", "libros", "usuarios" y "prestamos".
4. Se realizan tres consultas para obtener los libros más prestados, los autores más populares y los géneros más populares.

Este código crea una base de datos de biblioteca completa con tablas para libros, autores, géneros, usuarios y préstamos. También incluye datos de muestra para cada tabla. Las consultas al final del código muestran cómo obtener información útil de la base de datos, como los libros más prestados, los autores más populares y los géneros más populares.