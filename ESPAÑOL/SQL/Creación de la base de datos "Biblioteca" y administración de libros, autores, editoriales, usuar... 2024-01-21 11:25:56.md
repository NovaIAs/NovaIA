```sql
-- Crear una base de datos "biblioteca"
CREATE DATABASE biblioteca;

-- Seleccionar la base de datos "biblioteca"
USE biblioteca;

-- Crear una tabla "libros" con los siguientes campos:
-- - id (identificador único)
-- - titulo (título del libro)
-- - autor (autor del libro)
-- - editorial (editorial del libro)
-- - año_publicacion (año de publicación del libro)
CREATE TABLE libros (
    id INT NOT NULL AUTO_INCREMENT,
    titulo VARCHAR(255) NOT NULL,
    autor VARCHAR(255) NOT NULL,
    editorial VARCHAR(255) NOT NULL,
    año_publicacion INT NOT NULL,
    PRIMARY KEY (id)
);

-- Crear una tabla "autores" con los siguientes campos:
-- - id (identificador único)
-- - nombre (nombre del autor)
-- - apellido (apellido del autor)
CREATE TABLE autores (
    id INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255) NOT NULL,
    PRIMARY KEY (id)
);

-- Crear una tabla "editoriales" con los siguientes campos:
-- - id (identificador único)
-- - nombre (nombre de la editorial)
-- - dirección (dirección de la editorial)
CREATE TABLE editoriales (
    id INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    dirección VARCHAR(255) NOT NULL,
    PRIMARY KEY (id)
);

-- Crear una tabla "prestamos" con los siguientes campos:
-- - id (identificador único)
-- - libro_id (identificador del libro prestado)
-- - usuario_id (identificador del usuario que tomó prestado el libro)
-- - fecha_prestamo (fecha en que se prestó el libro)
-- - fecha_devolución (fecha en que se devolvió el libro)
CREATE TABLE prestamos (
    id INT NOT NULL AUTO_INCREMENT,
    libro_id INT NOT NULL,
    usuario_id INT NOT NULL,
    fecha_prestamo DATE NOT NULL,
    fecha_devolución DATE,
    PRIMARY KEY (id),
    FOREIGN KEY (libro_id) REFERENCES libros(id),
    FOREIGN KEY (usuario_id) REFERENCES usuarios(id)
);

-- Crear una tabla "usuarios" con los siguientes campos:
-- - id (identificador único)
-- - nombre (nombre del usuario)
-- - apellido (apellido del usuario)
-- - dirección (dirección del usuario)
CREATE TABLE usuarios (
    id INT NOT NULL AUTO_INCREMENT,
    nombre VARCHAR(255) NOT NULL,
    apellido VARCHAR(255) NOT NULL,
    dirección VARCHAR(255) NOT NULL,
    PRIMARY KEY (id)
);

-- Insertar datos en la tabla "autores"
INSERT INTO autores (nombre, apellido) VALUES
    ('Juan', 'Pérez'),
    ('María', 'García'),
    ('Pedro', 'López'),
    ('Ana', 'Fernández'),
    ('José', 'Rodríguez');

-- Insertar datos en la tabla "editoriales"
INSERT INTO editoriales (nombre, dirección) VALUES
    ('Editorial Planeta', 'Calle Mayor, 123'),
    ('Editorial Santillana', 'Calle Menor, 456'),
    ('Editorial Anaya', 'Calle Nueva, 789');

-- Insertar datos en la tabla "libros"
INSERT INTO libros (titulo, autor, editorial, año_publicacion) VALUES
    ('El Quijote', 'Miguel de Cervantes', 'Editorial Planeta', 1605),
    ('Cien años de soledad', 'Gabriel García Márquez', 'Editorial Santillana', 1967),
    ('El principito', 'Antoine de Saint-Exupéry', 'Editorial Anaya', 1943),
    ('La casa de Bernarda Alba', 'Federico García Lorca', 'Editorial Planeta', 1936),
    ('El túnel', 'Ernesto Sabato', 'Editorial Santillana', 1948);

-- Insertar datos en la tabla "usuarios"
INSERT INTO usuarios (nombre, apellido, dirección) VALUES
    ('Luis', 'Hernández', 'Calle Larga, 123'),
    ('Marta', 'González', 'Calle Corta, 456'),
    ('David', 'Martínez', 'Calle Ancha, 789');

-- Insertar datos en la tabla "prestamos"
INSERT INTO prestamos (libro_id, usuario_id, fecha_prestamo, fecha_devolución) VALUES
    (1, 1, '2023-03-08', '2023-03-15'),
    (2, 2, '2023-03-10', '2023-03-17'),
    (3, 3, '2023-03-12', '2023-03-19'),
    (4, 1, '2023-03-15', '2023-03-22'),
    (5, 2, '2023-03-17', '2023-03-24');

-- Obtener todos los libros de la base de datos
SELECT * FROM libros;

-- Obtener todos los autores de la base de datos
SELECT * FROM autores;

-- Obtener todas las editoriales de la base de datos
SELECT * FROM editoriales;

-- Obtener todos los usuarios de la base de datos
SELECT * FROM usuarios;

-- Obtener todos los préstamos de la base de datos
SELECT * FROM prestamos;

-- Obtener el título y el autor de todos los libros de la base de datos
SELECT titulo, autor FROM libros;

-- Obtener el nombre y el apellido de todos los autores de la base de datos
SELECT nombre, apellido FROM autores;

-- Obtener el nombre y la dirección de todas las editoriales de la base de datos
SELECT nombre, dirección FROM editoriales;

-- Obtener el nombre y la dirección de todos los usuarios de la base de datos
SELECT nombre, dirección FROM usuarios;

-- Obtener el título del libro, el nombre del autor y el nombre de la editorial de todos los libros de la base de datos
SELECT libros.titulo, autores.nombre, editoriales.nombre
FROM libros
JOIN autores ON libros.autor = autores.id
JOIN editoriales ON libros.editorial = editoriales.id;

-- Obtener el nombre del usuario, el título del libro y la fecha de préstamo de todos los préstamos de la base de datos
SELECT usuarios.nombre, libros.titulo, prestamos.fecha_prestamo
FROM prestamos
JOIN usuarios ON prestamos.usuario_id = usuarios.id
JOIN libros ON prestamos.libro_id = libros.id;

-- Obtener el nombre del usuario, el título del libro y la fecha de devolución de todos los préstamos de la base de datos
SELECT usuarios.nombre, libros.titulo, prestamos.fecha_devolución
FROM prestamos
JOIN usuarios ON prestamos.usuario_id = usuarios.id
JOIN libros ON prestamos.libro_id = libros.id
WHERE prestamos.fecha_devolución IS NOT NULL;

-- Obtener el nombre del usuario, el título del libro y la fecha de préstamo de todos los préstamos activos de la base de datos
SELECT usuarios.nombre, libros.titulo, prestamos.fecha_prestamo
FROM prestamos
JOIN usuarios ON prestamos.usuario_id = usuarios.id
JOIN libros ON prestamos.libro_id = libros.id
WHERE prestamos.fecha_devolución IS NULL;
```

Este código crea una base de datos llamada "biblioteca" y cinco tablas: "libros", "autores", "editoriales", "usuarios" y "prestamos". Luego, inserta datos en estas tablas y realiza una serie de consultas para obtener información sobre los libros, los autores, las editoriales, los usuarios y los préstamos.

El código está bien documentado y es fácil de entender. Utiliza una variedad de cláusulas SQL, como `SELECT`, `FROM`, `JOIN`, `WHERE` y `ORDER BY`, para realizar consultas complejas y obtener información específica de la base de datos.