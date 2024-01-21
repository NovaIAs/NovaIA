```sql
-- Creación de la base de datos "biblioteca"
CREATE DATABASE biblioteca;

-- Selección de la base de datos "biblioteca"
USE biblioteca;

-- Creación de la tabla "libros"
CREATE TABLE libros (
  id_libro INT NOT NULL AUTO_INCREMENT,
  titulo VARCHAR(255) NOT NULL,
  autor VARCHAR(255),
  editorial VARCHAR(255),
  año_publicacion INT,
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

-- Creación de la tabla "editoriales"
CREATE TABLE editoriales (
  id_editorial INT NOT NULL AUTO_INCREMENT,
  nombre VARCHAR(255) NOT NULL,
  direccion VARCHAR(255),
  telefono VARCHAR(255),
  correo_electronico VARCHAR(255),
  PRIMARY KEY (id_editorial)
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
  telefono VARCHAR(255),
  PRIMARY KEY (id_usuario)
);

-- Inserción de datos en la tabla "libros"
INSERT INTO libros (titulo, autor, editorial, año_publicacion, precio) VALUES
('El Quijote de la Mancha', 'Miguel de Cervantes Saavedra', 'Espasa', 1605, 12.99),
('Cien años de soledad', 'Gabriel García Márquez', 'HarperCollins', 1967, 14.99),
('El principito', 'Antoine de Saint-Exupéry', 'Salamandra', 1943, 9.99),
('El señor de los anillos', 'J.R.R. Tolkien', ' Minotauro', 1954, 30.99),
('Harry Potter y la piedra filosofal', 'J.K. Rowling', 'Salamandra', 1997, 16.99);

-- Inserción de datos en la tabla "autores"
INSERT INTO autores (nombre, apellido, nacionalidad, fecha_nacimiento) VALUES
('Miguel', 'de Cervantes Saavedra', 'España', '1547-09-29'),
('Gabriel', 'García Márquez', 'Colombia', '1927-03-06'),
('Antoine', 'de Saint-Exupéry', 'Francia', '1900-06-29'),
('J.R.R.', 'Tolkien', 'Reino Unido', '1892-01-03'),
('J.K.', 'Rowling', 'Reino Unido', '1965-07-31');

-- Inserción de datos en la tabla "editoriales"
INSERT INTO editoriales (nombre, direccion, telefono, correo_electronico) VALUES
('Espasa', 'Calle del Arenal, 11, 28013 Madrid', '915 21 99 00', 'info@espasa.es'),
('HarperCollins', '195 Broadway, New York, NY 10007, United States', '212-207-7000', 'info@harpercollins.com'),
('Salamandra', 'C/ Aragó, 385, 08013 Barcelona', '932 00 00 00', 'info@salamandra.com'),
('Minotauro', 'Passeig de Gràcia, 17, 08007 Barcelona', '933 17 20 00', 'info@minotauro.com');

-- Inserción de datos en la tabla "prestamos"
INSERT INTO prestamos (id_libro, id_usuario, fecha_prestamo, fecha_devolucion) VALUES
(1, 2, '2023-03-08', '2023-03-15'),
(2, 4, '2023-04-01', '2023-04-15'),
(3, 3, '2023-05-07', '2023-05-14'),
(4, 1, '2023-06-09', '2023-06-16'),
(5, 5, '2023-07-02', '2023-07-09');

-- Inserción de datos en la tabla "usuarios"
INSERT INTO usuarios (nombre, apellido, correo_electronico, telefono) VALUES
('Juan', 'Pérez', 'juan.perez@gmail.com', '600 123 456'),
('María', 'García', 'maria.garcia@hotmail.com', '699 234 567'),
('Pedro', 'López', 'pedro.lopez@yahoo.com', '611 345 678'),
('Ana', 'Fernández', 'ana.fernandez@gmail.com', '622 456 789'),
('José', 'Martínez', 'jose.martinez@hotmail.com', '633 567 890');

-- Consulta que muestra todos los libros de la tabla "libros"
SELECT * FROM libros;

-- Consulta que muestra todos los autores de la tabla "autores"
SELECT * FROM autores;

-- Consulta que muestra todas las editoriales de la tabla "editoriales"
SELECT * FROM editoriales;

-- Consulta que muestra todos los préstamos de la tabla "prestamos"
SELECT * FROM prestamos;

-- Consulta que muestra todos los usuarios de la tabla "usuarios"
SELECT * FROM usuarios;

-- Consulta que muestra todos los libros prestados por un usuario específico
SELECT l.titulo AS "Título del libro", a.nombre AS "Nombre del autor", u.nombre AS "Nombre del usuario", p.fecha_prestamo AS "Fecha de préstamo", p.fecha_devolucion AS "Fecha de devolución"
FROM prestamos p
JOIN libros l ON p.id_libro = l.id_libro
JOIN autores a ON l.id_autor = a.id_autor
JOIN usuarios u ON p.id_usuario = u.id_usuario
WHERE u.id_usuario = 1;

-- Consulta que muestra todos los autores que han escrito más de un libro
SELECT a.nombre AS "Nombre del autor", COUNT(l.id_libro) AS "Número de libros"
FROM autores a
JOIN libros l ON a.id_autor = l.id_autor
GROUP BY a.nombre
HAVING COUNT(l.id_libro) > 1;

-- Consulta que muestra todos los libros publicados en un año específico
SELECT l.titulo AS "Título del libro", a.nombre AS "Nombre del autor", e.nombre AS "Nombre de la editorial", l.año_publicacion AS "Año de publicación"
FROM libros l
JOIN autores a ON l.id_autor = a.id_autor
JOIN editoriales e ON l.id_editorial = e.id_editorial
WHERE l.año_publicacion = 2000;

-- Consulta que muestra todos los usuarios que han prestado un libro en un mes específico
SELECT u.nombre AS "Nombre del usuario", l.titulo AS "Título del libro", p.fecha_prestamo AS "Fecha de préstamo", p.fecha_devolucion AS "Fecha de devolución"
FROM prestamos p
JOIN libros l ON p.id_libro = l.id_libro
JOIN usuarios u ON p.id_usuario = u.id_usuario
WHERE strftime('%m', p.fecha_prestamo) = '03';
```

Explicación del código:

* La primera parte del código crea las tablas "libros", "autores", "editoriales", "prestamos" y "usuarios".
* La segunda parte del código inserta datos en las tablas "libros", "autores", "editoriales", "prestamos" y "usuarios".
* La tercera parte del código muestra consultas complejas que se pueden realizar en la base de datos.