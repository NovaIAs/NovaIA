```
Sistema para la gestión de una biblioteca

Paquetes:

- Biblioteca

Clases:

- Libro

  - Título
  - Autor
  - ISBN
  - Número de páginas
  - Fecha de publicación
  - Género
  - Disponibilidad

- Socio

  - Nombre
  - Apellidos
  - DNI
  - Dirección
  - Teléfono
  - Correo electrónico
  - Fecha de alta
  - Fecha de baja

- Préstamo

  - Libro
  - Socio
  - Fecha de préstamo
  - Fecha de devolución
  - Estado

- Multa

  - Socio
  - Importe
  - Fecha de imposición
  - Fecha de pago

Relaciones:

- Libro 1 : n Préstamo
- Socio 1 : n Préstamo
- Libro 1 : n Multa
- Socio 1 : n Multa

Diagrama de clases:

```
          +--------------------+
          | Libro              |
          +--------------------+
          | Título             |
          | Autor              |
          | ISBN               |
          | Número de páginas  |
          | Fecha de publicación |
          | Género             |
          | Disponibilidad     |
          +--------------------+

          +--------------------+
          | Socio              |
          +--------------------+
          | Nombre             |
          | Apellidos          |
          | DNI                |
          | Dirección          |
          | Teléfono           |
          | Correo electrónico |
          | Fecha de alta      |
          | Fecha de baja      |
          +--------------------+

          +--------------------+
          | Préstamo           |
          +--------------------+
          | Libro              |
          | Socio              |
          | Fecha de préstamo  |
          | Fecha de devolución |
          | Estado             |
          +--------------------+

          +--------------------+
          | Multa              |
          +--------------------+
          | Socio              |
          | Importe            |
          | Fecha de imposición |
          | Fecha de pago      |
          +--------------------+

          Libro           1 : n   Préstamo
          Socio            1 : n   Préstamo
          Libro           1 : n   Multa
          Socio            1 : n   Multa
```

El diagrama de clases muestra las clases y sus relaciones en el sistema para la gestión de una biblioteca. Las clases están representadas por rectángulos y las relaciones por líneas.

La clase Libro representa los libros de la biblioteca. Los atributos de la clase Libro son:

- Título: El título del libro.
- Autor: El autor del libro.
- ISBN: El ISBN del libro.
- Número de páginas: El número de páginas del libro.
- Fecha de publicación: La fecha de publicación del libro.
- Género: El género del libro.
- Disponibilidad: La disponibilidad del libro.

La clase Socio representa a los socios de la biblioteca. Los atributos de la clase Socio son:

- Nombre: El nombre del socio.
- Apellidos: Los apellidos del socio.
- DNI: El DNI del socio.
- Dirección: La dirección del socio.
- Teléfono: El teléfono del socio.
- Correo electrónico: El correo electrónico del socio.
- Fecha de alta: La fecha de alta del socio en la biblioteca.
- Fecha de baja: La fecha de baja del socio en la biblioteca.

La clase Préstamo representa los préstamos de libros de la biblioteca. Los atributos de la clase Préstamo son:

- Libro: El libro prestado.
- Socio: El socio que ha pedido prestado el libro.
- Fecha de préstamo: La fecha en que se ha realizado el préstamo.
- Fecha de devolución: La fecha en que se debe devolver el libro.
- Estado: El estado del préstamo.

La clase Multa representa las multas de la biblioteca. Los atributos de la clase Multa son:

- Socio: El socio que ha recibido la multa.
- Importe: El importe de la multa.
- Fecha de imposición: La fecha en que se ha impuesto la multa.
- Fecha de pago: La fecha en que se ha pagado la multa.

Las relaciones entre las clases se muestran con líneas. Las líneas están etiquetadas con el nombre de la relación y la cardinalidad de la relación. La cardinalidad de una relación indica el número de objetos de una clase que pueden estar relacionados con un objeto de otra clase.

La relación entre las clases Libro y Préstamo es una relación 1:n. Esto significa que un libro puede ser prestado por muchos socios, pero un socio solo puede prestar un libro.

La relación entre las clases Socio y Préstamo es una relación 1:n. Esto significa que un socio puede pedir prestado muchos libros, pero un libro solo puede ser prestado por un socio.

La relación entre las clases Libro y Multa es una relación 1:n. Esto significa que un libro puede tener muchas multas, pero una multa solo puede estar asociada a un libro.

La relación entre las clases Socio y Multa es una relación 1:n. Esto significa que un socio puede tener muchas multas, pero una multa solo puede estar asociada a un socio.

El diagrama de clases muestra las clases y sus relaciones en el sistema para la gestión de una biblioteca. Este diagrama puede usarse para crear una base de datos para el sistema o para generar código para el sistema.