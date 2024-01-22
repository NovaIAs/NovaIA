**Diagrama de Casos de Uso**

* **Actores:**
    * Usuario
    * Administrador
* **Casos de Uso:**
    * **Iniciar sesión:** El usuario ingresa su nombre de usuario y contraseña para acceder al sistema.
    * **Registrarse:** El usuario crea una nueva cuenta de usuario.
    * **Crear tarea:** El usuario crea una nueva tarea.
    * **Editar tarea:** El usuario edita una tarea existente.
    * **Eliminar tarea:** El usuario elimina una tarea existente.
    * **Completar tarea:** El usuario marca una tarea como completada.
    * **Ver tareas:** El usuario ve una lista de todas las tareas.
    * **Filtrar tareas:** El usuario filtra las tareas por estado, prioridad o fecha de vencimiento.
    * **Buscar tareas:** El usuario busca tareas por título o descripción.
    * **Administrar usuarios:** El administrador crea, edita y elimina usuarios.
    * **Restablecer contraseña:** El usuario restablece su contraseña.

**Diagrama de Clases**

* **Clase Usuario:**
    * Atributos:
        * Nombre de usuario
        * Contraseña
        * Correo electrónico
        * Nombre completo
    * Métodos:
        * Iniciar sesión
        * Registrarse
        * Crear tarea
        * Editar tarea
        * Eliminar tarea
        * Completar tarea
        * Ver tareas
        * Filtrar tareas
        * Buscar tareas
* **Clase Tarea:**
    * Atributos:
        * Título
        * Descripción
        * Estado
        * Prioridad
        * Fecha de vencimiento
    * Métodos:
        * Crear
        * Editar
        * Eliminar
        * Completar
        * Ver
        * Filtrar
        * Buscar
* **Clase Administrador:**
    * Atributos:
        * Nombre de usuario
        * Contraseña
    * Métodos:
        * Crear usuario
        * Editar usuario
        * Eliminar usuario
        * Restablecer contraseña

**Diagrama de Secuencia**

* **Iniciar sesión:**
    * El usuario ingresa su nombre de usuario y contraseña.
    * El sistema valida el nombre de usuario y la contraseña.
    * Si el nombre de usuario y la contraseña son válidos, el usuario es redirigido a la página principal.
    * Si el nombre de usuario o la contraseña son incorrectos, el usuario es notificado y se le solicita que vuelva a intentarlo.

* **Crear tarea:**
    * El usuario hace clic en el botón "Crear tarea".
    * El sistema muestra un formulario para crear una nueva tarea.
    * El usuario completa el formulario y hace clic en el botón "Guardar".
    * El sistema crea la nueva tarea y la agrega a la lista de tareas.

* **Editar tarea:**
    * El usuario hace clic en el botón "Editar" junto a la tarea que desea editar.
    * El sistema muestra un formulario para editar la tarea.
    * El usuario realiza los cambios necesarios y hace clic en el botón "Guardar".
    * El sistema guarda los cambios y actualiza la tarea en la lista de tareas.

* **Eliminar tarea:**
    * El usuario hace clic en el botón "Eliminar" junto a la tarea que desea eliminar.
    * El sistema muestra un mensaje de confirmación.
    * El usuario hace clic en el botón "Sí" para eliminar la tarea.
    * El sistema elimina la tarea de la lista de tareas.

* **Completar tarea:**
    * El usuario hace clic en el botón "Completar" junto a la tarea que desea completar.
    * El sistema marca la tarea como completada y la mueve a la lista de tareas completadas.

**Diagrama de Actividad**

* **Flujo de trabajo del usuario:**
    * El usuario inicia sesión en el sistema.
    * El usuario crea una nueva tarea.
    * El usuario edita una tarea existente.
    * El usuario elimina una tarea existente.
    * El usuario completa una tarea.
    * El usuario ve una lista de todas las tareas.
    * El usuario filtra las tareas por estado, prioridad o fecha de vencimiento.
    * El usuario busca tareas por título o descripción.

* **Flujo de trabajo del administrador:**
    * El administrador crea un nuevo usuario.
    * El administrador edita un usuario existente.
    * El administrador elimina un usuario existente.
    * El administrador restablece la contraseña de un usuario.

**Diagrama de Componentes**

* **Componente de interfaz de usuario:**
    * Este componente es responsable de la interfaz de usuario del sistema.
    * Incluye todos los elementos que el usuario ve y con los que interactúa, como botones, formularios, listas y cuadros de texto.
* **Componente de lógica de negocios:**
    * Este componente es responsable de la lógica de negocios del sistema.
    * Incluye todas las reglas y procesos que determinan cómo funciona el sistema.
* **Componente de acceso a datos:**
    * Este componente es responsable de acceder a los datos del sistema.
    * Incluye todas las clases y métodos que se utilizan para leer y escribir datos en la base de datos.

**Diagrama de Despliegue**

* **Servidor:**
    * Este componente es responsable de alojar el sistema.
    * Incluye el hardware y el software necesarios para ejecutar el sistema.
* **Cliente:**
    * Este componente es responsable de acceder al sistema.
    * Incluye el navegador web u otra aplicación que se utiliza para acceder al sistema.