```
Diagrama de Casos de Uso

Actores:
- Usuario
- Administrador

Casos de uso:
- Iniciar sesión
- Registrarse
- Crear una publicación
- Editar una publicación
- Eliminar una publicación
- Ver publicaciones
- Buscar publicaciones
- Comentar una publicación
- Me gusta en una publicación
- Compartir una publicación

Diagrama de Clases

Clases:
- Usuario
- Administrador
- Publicación
- Comentario
- Me gusta
- Compartido

Atributos:
- Usuario: nombre de usuario, contraseña, correo electrónico
- Administrador: nombre de usuario, contraseña, correo electrónico, roles
- Publicación: título, contenido, fecha de creación, autor
- Comentario: contenido, fecha de creación, autor
- Me gusta: fecha y hora, autor
- Compartido: fecha y hora, autor

Métodos:
- Usuario: iniciar sesión, registrarse, crear una publicación, editar una publicación, eliminar una publicación, ver publicaciones, buscar publicaciones, comentar una publicación, me gusta en una publicación, compartir una publicación
- Administrador: iniciar sesión, registrarse, crear una publicación, editar una publicación, eliminar una publicación, ver publicaciones, buscar publicaciones, comentar una publicación, me gusta en una publicación, compartir una publicación, gestionar usuarios, gestionar publicaciones
- Publicación: obtener título, obtener contenido, obtener fecha de creación, obtener autor, establecer título, establecer contenido, establecer fecha de creación, establecer autor
- Comentario: obtener contenido, obtener fecha de creación, obtener autor, establecer contenido, establecer fecha de creación, establecer autor
- Me gusta: obtener fecha y hora, obtener autor, establecer fecha y hora, establecer autor
- Compartido: obtener fecha y hora, obtener autor, establecer fecha y hora, establecer autor

Diagrama de Secuencia

Caso de uso: Iniciar sesión

1. El usuario introduce su nombre de usuario y contraseña.
2. El sistema comprueba si el nombre de usuario y la contraseña son válidos.
3. Si el nombre de usuario y la contraseña son válidos, el sistema inicia la sesión del usuario.
4. Si el nombre de usuario y la contraseña no son válidos, el sistema muestra un mensaje de error.

Diagrama de Comunicación

Caso de uso: Crear una publicación

1. El usuario hace clic en el botón "Crear una publicación".
2. El sistema muestra un formulario para crear una publicación.
3. El usuario introduce el título y el contenido de la publicación.
4. El usuario hace clic en el botón "Publicar".
5. El sistema crea la publicación y la almacena en la base de datos.
6. El sistema muestra la publicación en la página de inicio.

Diagrama de Estado

Caso de uso: Estado de la publicación

1. Nueva: La publicación se ha creado, pero aún no se ha publicado.
2. Publicada: La publicación se ha publicado y es visible para todos los usuarios.
3. Editando: La publicación se está editando.
4. Eliminada: La publicación se ha eliminado.

Diagrama de Actividad

Caso de uso: Proceso de publicación

1. El usuario crea una publicación.
2. El sistema comprueba si la publicación es válida.
3. Si la publicación es válida, el sistema la publica.
4. Si la publicación no es válida, el sistema muestra un mensaje de error al usuario.
5. El usuario puede editar o eliminar la publicación.

Explicación del código:

El código anterior es un ejemplo de un diseño UML para una aplicación web de redes sociales. El diagrama de casos de uso muestra los diferentes tipos de interacciones que los usuarios pueden tener con el sistema. El diagrama de clases muestra las diferentes clases de objetos que existen en el sistema. El diagrama de secuencia muestra la secuencia de pasos que se siguen cuando un usuario inicia sesión en el sistema. El diagrama de comunicación muestra los diferentes tipos de mensajes que se intercambian entre los objetos del sistema. El diagrama de estado muestra los diferentes estados en los que una publicación puede estar. El diagrama de actividad muestra el proceso de publicación de una publicación.

Este diseño UML es sólo un ejemplo y puede modificarse para adaptarse a las necesidades específicas de una aplicación web de redes sociales. Sin embargo, proporciona una base sólida para el desarrollo de una aplicación web de redes sociales segura y escalable.
```