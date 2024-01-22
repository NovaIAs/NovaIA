```
-- Crear una nueva base de datos llamada "biblioteca"
CREATE DATABASE biblioteca;

-- Seleccionar la base de datos "biblioteca" como la base de datos actual
USE biblioteca;

-- Crear una tabla llamada "libros" para almacenar información sobre los libros en la biblioteca
CREATE TABLE libros (
  id INT NOT NULL AUTO_INCREMENT,
  titulo VARCHAR(255) NOT NULL,
  autor VARCHAR(255) NOT NULL,
  editorial VARCHAR(255),
  año_publicacion INT,
  precio DECIMAL(10, 2),
  PRIMARY KEY (id)
);

-- Crear una tabla llamada "autores" para almacenar información sobre los autores de los libros
CREATE TABLE autores (
  id INT NOT NULL AUTO_INCREMENT,
  nombre VARCHAR(255) NOT NULL,
  nacionalidad VARCHAR(255),
  fecha_nacimiento DATE,
  PRIMARY KEY (id)
);

-- Crear una tabla llamada "editoriales" para almacenar información sobre las editoriales de los libros
CREATE TABLE editoriales (
  id INT NOT NULL AUTO_INCREMENT,
  nombre VARCHAR(255) NOT NULL,
  direccion VARCHAR(255),
  telefono VARCHAR(255),
  PRIMARY KEY (id)
);

-- Crear una tabla llamada "prestamos" para almacenar información sobre los préstamos de libros
CREATE TABLE prestamos (
  id INT NOT NULL AUTO_INCREMENT,
  libro_id INT NOT NULL,
  lector_id INT NOT NULL,
  fecha_prestamo DATE,
  fecha_devolucion DATE,
  PRIMARY KEY (id),
  FOREIGN KEY (libro_id) REFERENCES libros(id),
  FOREIGN KEY (lector_id) REFERENCES lectores(id)
);

-- Crear una tabla llamada "lectores" para almacenar información sobre los lectores de la biblioteca
CREATE TABLE lectores (
  id INT NOT NULL AUTO_INCREMENT,
  nombre VARCHAR(255) NOT NULL,
  apellido VARCHAR(255),
  direccion VARCHAR(255),
  telefono VARCHAR(255),
  PRIMARY KEY (id)
);

-- Insertar algunos datos iniciales en la tabla "libros"
INSERT INTO libros (titulo, autor, editorial, año_publicacion, precio) VALUES
  ('El Quijote', 'Miguel de Cervantes Saavedra', 'Espasa', 1605, 20.00),
  ('Cien años de soledad', 'Gabriel García Márquez', 'Penguin Random House', 1967, 15.00),
  ('El principito', 'Antoine de Saint-Exupéry', 'Salamandra', 1943, 10.00),
  ('El señor de los anillos', 'J.R.R. Tolkien', 'HarperCollins', 1954, 30.00),
  ('Harry Potter y la piedra filosofal', 'J.K. Rowling', 'Bloomsbury', 1997, 12.00);

-- Insertar algunos datos iniciales en la tabla "autores"
INSERT INTO autores (nombre, nacionalidad, fecha_nacimiento) VALUES
  ('Miguel de Cervantes Saavedra', 'España', '1547-09-29'),
  ('Gabriel García Márquez', 'Colombia', '1927-03-06'),
  ('Antoine de Saint-Exupéry', 'Francia', '1900-06-29'),
  ('J.R.R. Tolkien', 'Reino Unido', '1892-01-03'),
  ('J.K. Rowling', 'Reino Unido', '1965-07-31');

-- Insertar algunos datos iniciales en la tabla "editoriales"
INSERT INTO editoriales (nombre, direccion, telefono) VALUES
  ('Espasa', 'Calle Gran Vía, 48, 28013 Madrid', '915 22 90 00'),
  ('Penguin Random House', 'Calle Sant Jordi, 20, 08001 Barcelona', '934 92 90 00'),
  ('Salamandra', 'Calle Aragó, 385, 08013 Barcelona', '932 72 01 00'),
  ('HarperCollins', 'Calle Bretón de los Herreros, 23, 28003 Madrid', '915 35 01 00'),
  ('Bloomsbury', 'Calle Trafalgar Square, 15, Londres WC2N 5EB', '+44 20 7369 1000');

-- Insertar algunos datos iniciales en la tabla "lectores"
INSERT INTO lectores (nombre, apellido, direccion, telefono) VALUES
  ('Juan', 'Pérez', 'Calle Mayor, 123, 28013 Madrid', '915 22 90 01'),
  ('María', 'García', 'Calle Valencia, 456, 08013 Barcelona', '934 92 90 02'),
  ('Pedro', 'López', 'Calle Aragón, 789, 28013 Madrid', '915 35 01 03'),
  ('Ana', 'Fernández', 'Calle Sevilla, 123, 08013 Barcelona', '932 72 01 04'),
  ('José', 'Rodríguez', 'Calle Toledo, 456, 28013 Madrid', '915 22 90 05');

-- Insertar algunos datos iniciales en la tabla "prestamos"
INSERT INTO prestamos (libro_id, lector_id, fecha_prestamo, fecha_devolucion) VALUES
  (1, 1, '2023-03-08', '2023-03-15'),
  (2, 2, '2023-03-10', '2023-03-17'),
  (3, 3, '2023-03-12', '2023-03-19'),
  (4, 4, '2023-03-14', '2023-03-21'),
  (5, 5, '2023-03-16', '2023-03-23');

-- Consultar todos los libros de la tabla "libros"
SELECT * FROM libros;

-- Consultar todos los autores de la tabla "autores"
SELECT * FROM autores;

-- Consultar todos los lectores de la tabla "lectores"
SELECT * FROM lectores;

-- Consultar los libros que han sido prestados por el lector con ID 1 (Juan Pérez)
SELECT * FROM prestamos WHERE lector_id = 1;

-- Consultar los lectores que han prestado el libro con ID 1 (El Quijote)
SELECT * FROM prestamos WHERE libro_id = 1;

-- Consultar todos los libros que tienen un precio mayor a 20€
SELECT * FROM libros WHERE precio > 20;

-- Consultar todos los autores que han nacido antes de 1900
SELECT * FROM autores WHERE fecha_nacimiento < '1900-01-01';

-- Consultar todos los libros publicados en el año 1954
SELECT * FROM libros WHERE año_publicacion = 1954;

-- Consultar todos los lectores que viven en la calle Mayor
SELECT * FROM lectores WHERE direccion LIKE '%Calle Mayor%'s

-- Consultar todos los libros que tienen como autor a Miguel de Cervantes Saavedra o a Gabriel García Márquez
SELECT * FROM libros WHERE autor IN ('Miguel de Cervantes Saavedra', 'Gabriel García Márquez');

-- Consultar todos los prestamos realizados en el mes de marzo de 2023
SELECT * FROM prestamos WHERE fecha_prestamo LIKE 1`2023-03%'">
```

Explicación del código:

* El código comienza creando una nueva base de datos llamada "biblioteca" y seleccionándola como la base de datos actual.
* A continuación, se crean cinco tablas: "libros", "autores", "editoriales", "lectores" y "prestamos".
* Cada tabla tiene sus propios campos y restricciones. Por ejemplo, la tabla "libros" tiene un campo llamado "id" que es clave primaria y autoincremental, un campo llamado "titulo" que es obligatorio y no puede tener más de 255 caracteres, etc.
* A continuación, se insertan algunos datos iniciales en cada una de las tablas. Estos datos son solo ejemplos y pueden ser modificados o eliminados según las necesidades.
* Finalmente, se ejecutan varias consultas SQL para mostrar diferentes tipos de información sobre los libros, autores, lectores y préstamos. Por ejemplo, la consulta "SELECT * FROM libros;" muestra todos los libros de la tabla "libros".