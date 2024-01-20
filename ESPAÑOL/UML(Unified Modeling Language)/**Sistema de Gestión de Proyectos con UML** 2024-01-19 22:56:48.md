```
+------------------------------------------------------------------------------------+
|                                 Sistema de Gestión de Proyectos                        |
|                                                                                    |
+------------------------------------------------------------------------------------+

+------------------------------------------------------------------------------------+
|                                                                                    |
|                                    Clases                                           |
|                                                                                    |
+------------------------------------------------------------------------------------+

+------------------------------------------------------------------------------------+
| Nombre: Proyecto                                                                  |
|                                                                                    |
| Atributos:                                                                       |
|  - ID                                                                               |
|  - Nombre                                                                           |
|  - FechaInicio                                                                      |
|  - FechaFin                                                                         |
|  - Estado                                                                           |
|                                                                                    |
| Métodos:                                                                           |
|  - Crear()                                                                          |
|  - Actualizar()                                                                     |
|  - Eliminar()                                                                       |
|  - Obtener()                                                                         |
|                                                                                    |
+------------------------------------------------------------------------------------+

+------------------------------------------------------------------------------------+
| Nombre: Tarea                                                                      |
|                                                                                    |
| Atributos:                                                                       |
|  - ID                                                                               |
|  - Nombre                                                                           |
|  - Descripción                                                                      |
|  - FechaInicio                                                                      |
|  - FechaFin                                                                         |
|  - Estado                                                                           |
|  - ProyectoID                                                                       |
|                                                                                    |
| Métodos:                                                                           |
|  - Crear()                                                                          |
|  - Actualizar()                                                                     |
|  - Eliminar()                                                                       |
|  - Obtener()                                                                         |
|                                                                                    |
+------------------------------------------------------------------------------------+

+------------------------------------------------------------------------------------+
| Nombre: Recurso                                                                     |
|                                                                                    |
| Atributos:                                                                       |
|  - ID                                                                               |
|  - Nombre                                                                           |
|  - Rol                                                                              |
|  - ProyectoID                                                                       |
|                                                                                    |
| Métodos:                                                                           |
|  - Crear()                                                                          |
|  - Actualizar()                                                                     |
|  - Eliminar()                                                                       |
|  - Obtener()                                                                         |
|                                                                                    |
+------------------------------------------------------------------------------------+

+------------------------------------------------------------------------------------+
|                                                                                    |
|                                    Diagramas                                        |
|                                                                                    |
+------------------------------------------------------------------------------------+

+------------------------------------------------------------------------------------+
| Diagrama de Clases                                                                |
|                                                                                    |
+------------------------------------------------------------------------------------+

```
Proyecto
  - ID
  - Nombre
  - FechaInicio
  - FechaFin
  - Estado

Tarea
  - ID
  - Nombre
  - Descripción
  - FechaInicio
  - FechaFin
  - Estado
  - ProyectoID

Recurso
  - ID
  - Nombre
  - Rol
  - ProyectoID

```

+------------------------------------------------------------------------------------+
| Diagrama de Casos de Uso                                                         |
|                                                                                    |
+------------------------------------------------------------------------------------+

```
Actor: Administrador
  - Crear Proyecto
  - Actualizar Proyecto
  - Eliminar Proyecto
  - Obtener Proyecto

Actor: Usuario
  - Crear Tarea
  - Actualizar Tarea
  - Eliminar Tarea
  - Obtener Tarea

Actor: Recurso
  - Crear Recurso
  - Actualizar Recurso
  - Eliminar Recurso
  - Obtener Recurso

```

+------------------------------------------------------------------------------------+
| Diagrama de Secuencia                                                             |
|                                                                                    |
+------------------------------------------------------------------------------------+

```
Crear Proyecto
  - Administrador envía una solicitud para crear un proyecto.
  - El sistema valida la solicitud y crea el proyecto.
  - El sistema envía una notificación al administrador con el ID del proyecto creado.

Actualizar Proyecto
  - Administrador envía una solicitud para actualizar un proyecto.
  - El sistema valida la solicitud y actualiza el proyecto.
  - El sistema envía una notificación al administrador con el ID del proyecto actualizado.

Eliminar Proyecto
  - Administrador envía una solicitud para eliminar un proyecto.
  - El system valida la solicitud y elimina el proyecto.
  - El sistema envía una notificación al administrador con el ID del proyecto eliminado.

Obtener Proyecto
  - Administrador envía una solicitud para obtener un proyecto.
  - El sistema valida la solicitud y obtiene el proyecto.
  - El system envía el proyecto al administrador.

Crear Tarea
  - Usuario envía una solicitud para crear una tarea.
  - El sistema valida la solicitud y crea la tarea.
  - El sistema envía una notificación al usuario con el ID de la tarea creada.

Actualizar Tarea
  - Usuario envía una solicitud para actualizar una tarea.
  - El sistema valida la solicitud y actualiza la tarea.
  - El sistema envía una notificación al usuario con el ID de la tarea actualizada.

Eliminar Tarea
  - Usuario envía una solicitud para eliminar una tarea.
  - El sistema valida la solicitud y elimina la tarea.
  - El sistema envía una notificación al usuario con el ID de la tarea eliminada.

Obtener Tarea
  - Usuario envía una solicitud para obtener una tarea.
  - El sistema valida la solicitud y obtiene la tarea.
  - El sistema envía la tarea al usuario.

Crear Recurso
  - Recurso envía una solicitud para crear un recurso.
  - El sistema valida la solicitud y crea el recurso.
  - El system envía una notificación al recurso con el ID del recurso creado.

Actualizar Recurso
  - Recurso envía una solicitud para actualizar un recurso.
  - El sistema valida la solicitud y actualiza el recurso.
  - El system envía una notificación al recurso con el ID del recurso actualizado.

Eliminar Recurso
  - Recurso envía una solicitud para eliminar un recurso.
  - El system valida la solicitud y elimina el recurso.
  - El system envía una notificación al recurso con el ID del recurso eliminado.

Obtener Recurso
  - Recurso envía una solicitud para obtener un recurso.
  - El system valida la solicitud y obtiene el recurso.
  - El system envía el recurso al recurso.

```

+------------------------------------------------------------------------------------+
| Exlicación                                                                         |
|                                                                                    |
+------------------------------------------------------------------------------------+

El código UML anterior describe un sistema de gestión de proyectos. El sistema permite a los administradores crear, actualizar y eliminar proyectos. También permite a los usuarios crear, actualizar y eliminar tareas. Además, permite a los recursos crear, actualizar y eliminar recursos.

El diagrama de clases muestra las clases del sistema. El diagrama de casos de uso muestra los casos de uso del sistema. El diagrama de secuencia muestra las secuencias de los casos de uso.

El código UML anterior es un ejemplo de cómo se puede utilizar UML para modelar un sistema de gestión de proyectos.