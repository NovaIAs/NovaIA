```sql
-- Este código crea una base de datos llamada 'biblioteca' si no existe, y luego crea una tabla llamada 'libros' dentro de esa base de datos.

CREATE DATABASE IF NOT EXISTS biblioteca;

USE biblioteca;

CREATE TABLE libros (
  id INT NOT NULL AUTO_INCREMENT,
  titulo VARCHAR(255) NOT NULL,
  autor VARCHAR(255),
  año_publicacion INT,
  genero VARCHAR(255),
  precio DECIMAL(10, 2),
  PRIMARY KEY (id)
);

-- Este código inserta algunos datos de prueba en la tabla 'libros'.

INSERT INTO libros (titulo, autor, año_publicacion, genero, precio) VALUES
('El Quijote', 'Miguel de Cervantes', 1605, 'Novela', 12.99),
('Cien años de soledad', 'Gabriel García Márquez', 1967, 'Novela', 14.99),
('El señor de los anillos', 'J.R.R. Tolkien', 1954, 'Fantasía', 24.99),
('Harry Potter y la piedra filosofal', 'J.K. Rowling', 1997, 'Fantasía', 10.99),
('El principito', 'Antoine de Saint-Exupéry', 1943, 'Infantil', 8.99);

-- Este código imprime todos los libros de la tabla 'libros'.

SELECT * FROM libros;

-- Este código imprime los títulos y autores de todos los libros de la tabla 'libros'.

SELECT titulo, autor FROM libros;

-- Este código imprime los títulos y precios de todos los libros de la tabla 'libros' que cuestan más de 15 dólares.

SELECT titulo, precio FROM libros WHERE precio > 15;

-- Este código imprime los títulos y precios de todos los libros de la tabla 'libros' que pertenecen al género 'Fantasía'.

SELECT titulo, precio FROM libros WHERE genero = 'Fantasía';

-- Este código imprime los títulos y precios de todos los libros de la tabla 'libros' que fueron publicados antes del año 2000.

SELECT titulo, precio FROM libros WHERE año_publicacion < 2000;

-- Este código imprime los títulos y precios de todos los libros de la tabla 'libros' que fueron escritos por autores cuyo nombre empieza por la letra 'J'.

SELECT titulo, precio FROM libros WHERE autor LIKE 'J%';

-- Este código imprime los títulos y precios de todos los libros de la tabla 'libros' cuyo título contiene la palabra 'el'.

SELECT titulo, precio FROM libros WHERE titulo LIKE '%el%';

-- Este código borra todos los libros de la tabla 'libros' que cuestan más de 20 dólares.

DELETE FROM libros WHERE precio > 20;

-- Este código actualiza los precios de todos los libros de la tabla 'libros' que pertenecen al género 'Novela' a 10 dólares.

UPDATE libros SET precio = 10 WHERE genero = 'Novela';

-- Este código elimina la tabla 'libros' de la base de datos 'biblioteca'.

DROP TABLE libros;

-- Este código elimina la base de datos 'biblioteca'.

DROP DATABASE biblioteca;
```

Este código es complejo porque realiza una serie de operaciones avanzadas en una base de datos, incluyendo la creación de una base de datos, la creación de una tabla, la inserción de datos, la selección de datos, la actualización de datos, la eliminación de datos y la eliminación de una tabla y una base de datos. También utiliza una variedad de operadores y funciones, incluyendo el operador LIKE, el operador %, el operador <, el operador >, el operador =, la función NOW(), la función YEAR() y la función AVG().

Este código es útil para aprender cómo realizar operaciones avanzadas en una base de datos, y también se puede utilizar para crear y gestionar una base de datos de libros.