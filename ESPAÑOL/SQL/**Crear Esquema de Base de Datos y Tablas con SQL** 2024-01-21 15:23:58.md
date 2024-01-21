```sql
-- Define el esquema de la base de datos

CREATE SCHEMA `miclase` DEFAULT CHARACTER SET utf8 ;

-- Crea una tabla llamada "alumnos" con información de los alumnos

CREATE TABLE `miclase`.`alumnos` (
  `id_alumno` INT NOT NULL AUTO_INCREMENT,
  `nombre` VARCHAR(50) NOT NULL,
  `apellido` VARCHAR(50) NOT NULL,
  `email` VARCHAR(100) NOT NULL,
  `fecha_nacimiento` DATE NOT NULL,
  `sexo` ENUM('M', 'F') NOT NULL,
  PRIMARY KEY (`id_alumno`)
) ENGINE=InnoDB ;

-- Crea una tabla llamada "cursos" con información de los cursos

CREATE TABLE `miclase`.`cursos` (
  `id_curso` INT NOT NULL AUTO_INCREMENT,
  `nombre` VARCHAR(100) NOT NULL,
  `descripcion` TEXT NOT NULL,
  `fecha_inicio` DATE NOT NULL,
  `fecha_fin` DATE NOT NULL,
  `precio` DECIMAL(10, 2) NOT NULL,
  PRIMARY KEY (`id_curso`)
) ENGINE=InnoDB ;

-- Crea una tabla llamada "matriculas" con información de las matrículas

CREATE TABLE `miclase`.`matriculas` (
  `id_matricula` INT NOT NULL AUTO_INCREMENT,
  `id_alumno` INT NOT NULL,
  `id_curso` INT NOT NULL,
  `fecha_matricula` DATE NOT NULL,
  PRIMARY KEY (`id_matricula`),
  FOREIGN KEY (`id_alumno`) REFERENCES `miclase`.`alumnos`(`id_alumno`),
  FOREIGN KEY (`id_curso`) REFERENCES `miclase`.`cursos`(`id_curso`)
) ENGINE=InnoDB ;

-- Inserta datos en la tabla "alumnos"

INSERT INTO `miclase`.`alumnos` (`nombre`, `apellido`, `email`, `fecha_nacimiento`, `sexo`) VALUES
('Juan', 'Pérez', 'juan.perez@gmail.com', '2000-01-01', 'M'),
('María', 'González', 'maria.gonzalez@hotmail.com', '2001-02-02', 'F'),
('Pedro', 'Rodríguez', 'pedro.rodriguez@yahoo.es', '2002-03-03', 'M'),
('Ana', 'Fernández', 'ana.fernandez@gmail.com', '2003-04-04', 'F'),
('Luis', 'López', 'luis.lopez@hotmail.com', '2004-05-05', 'M');

-- Inserta datos en la tabla "cursos"

INSERT INTO `miclase`.`cursos` (`nombre`, `descripcion`, `fecha_inicio`, `fecha_fin`, `precio`) VALUES
('Matemáticas', 'Curso de matemáticas básicas', '2023-02-13', '2023-05-12', 100.00),
('Física', 'Curso de física general', '2023-03-06', '2023-06-05', 150.00),
('Química', 'Curso de química general', '2023-04-03', '2023-07-02', 120.00),
('Biología', 'Curso de biología general', '2023-05-01', '2023-08-01', 130.00),
('Historia', 'Curso de historia universal', '2023-06-01', '2023-09-01', 110.00);

-- Inserta datos en la tabla "matriculas"

INSERT INTO `miclase`.`matriculas` (`id_alumno`, `id_curso`, `fecha_matricula`) VALUES
(1, 1, '2023-02-15'),
(1, 2, '2023-03-15'),
(2, 1, '2023-02-20'),
(2, 3, '2023-04-10'),
(3, 2, '2023-03-25'),
(3, 4, '2023-05-15'),
(4, 3, '2023-04-20'),
(4, 5, '2023-06-05'),
(5, 4, '2023-05-25');

-- Consulta SQL para obtener el nombre y apellido de los alumnos junto con el nombre del curso y el precio del curso en el que están matriculados

SELECT
  a.nombre AS 'Nombre del Alumno',
  a.apellido AS 'Apellido del Alumno',
  c.nombre AS 'Nombre del Curso',
  c.precio AS 'Precio del Curso'
FROM
  alumnos a
JOIN
  matriculas m ON a.id_alumno = m.id_alumno
JOIN
  cursos c ON m.id_curso = c.id_curso;
```

Explicación del código:

1. Creamos el esquema de la base de datos llamado "miclase" con el comando `CREATE SCHEMA`.
2. Creamos tres tablas: "alumnos", "cursos" y "matriculas" con el comando `CREATE TABLE`. Cada tabla tiene sus propios campos y tipos de datos.
3. Insertamos datos de prueba en las tablas "alumnos", "cursos" y "matriculas" con el comando `INSERT INTO`.
4. Finalmente, ejecutamos una consulta SQL que recupera el nombre y apellido de los alumnos junto con el nombre del curso y el precio del curso en el que están matriculados. Para ello, utilizamos las cláusulas `JOIN` y `ON` para relacionar las tablas "alumnos", "matriculas" y "cursos".

Este código es complejo porque:

* Utiliza múltiples tablas y relaciones entre ellas.
* Utiliza una combinación de comandos DDL (Data Definition Language) y DML (Data Manipulation Language).
* Incluye una consulta SQL compleja que utiliza cláusulas `JOIN` y `ON` para recuperar datos de múltiples tablas.