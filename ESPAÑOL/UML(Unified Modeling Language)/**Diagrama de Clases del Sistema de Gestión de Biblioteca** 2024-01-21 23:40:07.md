**Diagrama de Clases**

```
+-------------------------------------------------+
|                                                 |
|                                                 |
|             +---------------------------+        |
|             |                           |        |
|             |                           |        |
|             |                           |        |
|             |                           |        |
|             |                           |        |
|             +---------------------------+        |
|                                                 |
|                                                 |
+-------------------------------------------------+

**Clase:** Sistema de Gestión de Biblioteca

**Atributos:**

* **Libros:** Colección de libros en la biblioteca.
* **Usuarios:** Colección de usuarios registrados en la biblioteca.
* **Préstamos:** Colección de préstamos activos.
* **Devoluciones:** Colección de devoluciones activas.
* **Multas:** Colección de multas activas.
* **Configuraciones:** Colección de configuraciones del sistema.

**Métodos:**

* **AgregarLibro:** Agrega un nuevo libro a la colección de libros.
* **EliminarLibro:** Elimina un libro de la colección de libros.
* **ActualizarLibro:** Actualiza la información de un libro.
* **AgregarUsuario:** Agrega un nuevo usuario a la colección de usuarios.
* **EliminarUsuario:** Elimina un usuario de la colección de usuarios.
* **ActualizarUsuario:** Actualiza la información de un usuario.
* **AgregarPréstamo:** Agrega un nuevo préstamo a la colección de préstamos.
* **EliminarPréstamo:** Elimina un préstamo de la colección de préstamos.
* **ActualizarPréstamo:** Actualiza la información de un préstamo.
* **AgregarDevolución:** Agrega una nueva devolución a la colección de devoluciones.
* **EliminarDevolución:** Elimina una devolución de la colección de devoluciones.
* **ActualizarDevolución:** Actualiza la información de una devolución.
* **AgregarMulta:** Agrega una nueva multa a la colección de multas.
* **EliminarMulta:** Elimina una multa de la colección de multas.
* **ActualizarMulta:** Actualiza la información de una multa.
* **AgregarConfiguración:** Agrega una nueva configuración al sistema.
* **EliminarConfiguración:** Elimina una configuración del sistema.
* **ActualizarConfiguración:** Actualiza la información de una configuración.

**Relaciones:**

* **Libros** se relaciona con **Préstamos** a través de la relación **PréstamoLibro**.
* **Libros** se relaciona con **Devoluciones** a través de la relación **DevoluciónLibro**.
* **Libros** se relaciona con **Multas** a través de la relación **MultaLibro**.
* **Usuarios** se relaciona con **Préstamos** a través de la relación **PréstamoUsuario**.
* **Usuarios** se relaciona con **Devoluciones** a través de la relación **DevoluciónUsuario**.
* **Usuarios** se relaciona con **Multas** a través de la relación **MultaUsuario**.
* **Configuraciones** se relaciona con **Sistema de Gestión de Biblioteca** a través de la relación **ConfiguraciónSistema**.

**Diagrama de Secuencia**

```
+-------------------------------------------------+
|                                                 |
|                                                 |
|   +----------------------------+   +------------+   |
|   |                           |   |            |   |
|   |                           |   |            |   |
|   |                           |   |            |   |
|   |                           |   |            |   |
|   |                           |   |            |   |
|   +----------------------------+   +------------+   |
|                                                 |
|                                                 |
+-------------------------------------------------+

**Objeto:** Sistema de Gestión de Biblioteca

**Mensaje:** AgregarLibro

**Origen:** Bibliotecario

**Destino:** Sistema de Gestión de Biblioteca

**Parámetros:**

* **Libro:** El libro a agregar.

**Diagrama de Actividad**

```
+-------------------------------------------------+
|                                                 |
|                                                 |
|   +----------------------------+   +------------+   |
|   |                           |   |            |   |
|   |                           |   |            |   |
|   |                           |   |            |   |
|   |                           |   |            |   |
|   |                           |   |            |   |
|   +----------------------------+   +------------+   |
|                                                 |
|                                                 |
+-------------------------------------------------+

**Actividad:** Préstamo de Libro

**Inicio:** Bibliotecario escanea la tarjeta de biblioteca del usuario.

**Fin:** Bibliotecario imprime el recibo de préstamo.

**Actividades Intermedias:**

* Bibliotecario busca el libro en el catálogo.
* Bibliotecario verifica que el libro no esté prestado.
* Bibliotecario registra el préstamo en el sistema.
* Bibliotecario devuelve el libro al usuario.

**Decisiones:**

* ¿El libro está prestado?
* ¿El usuario tiene multas pendientes?

**Flujos de Control:**

* Si el libro está prestado, el bibliotecario notifica al usuario y finaliza el proceso.
* Si el usuario tiene multas pendientes, el bibliotecario notifica al usuario y finaliza el proceso.
* Si el libro no está prestado y el usuario no tiene multas pendientes, el bibliotecario registra el préstamo en el sistema y devuelve el libro al usuario.