```
Sistema de Gestión de Proyectos (SGP)

```
*Modelo de Negocio*

* **Proyecto:**
    * Identificador (ID)
    * Nombre
    * Descripción
    * Fecha de inicio
    * Fecha de finalización
    * Presupuesto
    * Estado
* **Tarea:**
    * Identificador (ID)
    * Nombre
    * Descripción
    * Fecha de inicio
    * Fecha de finalización
    * Duración
    * Dependencias
    * Estado
* **Recurso:**
    * Identificador (ID)
    * Nombre
    * Tipo
    * Disponibilidad
    * Coste

```
*Modelo de Datos*

```
* **Tabla Proyectos:**
    * ID_Proyecto (clave primaria)
    * Nombre
    * Descripción
    * Fecha_Inicio
    * Fecha_Finalización
    * Presupuesto
    * Estado
* **Tabla Tareas:**
    * ID_Tarea (clave primaria)
    * Nombre
    * Descripción
    * Fecha_Inicio
    * Fecha_Finalización
    * Duración
    * Dependencias
    * Estado
* **Tabla Recursos:**
    * ID_Recurso (clave primaria)
    * Nombre
    * Tipo
    * Disponibilidad
    * Coste

```
*Modelo de Procesos*

```
* **Creación de un proyecto:**
    1. El usuario ingresa el nombre, la descripción, la fecha de inicio, la fecha de finalización y el presupuesto del proyecto.
    2. El sistema genera un ID único para el proyecto.
    3. El proyecto se almacena en la tabla Proyectos.
* **Asignación de tareas a un proyecto:**
    1. El usuario selecciona el proyecto al que desea asignar tareas.
    2. El usuario ingresa el nombre, la descripción, la fecha de inicio, la fecha de finalización, la duración, las dependencias y el estado de la tarea.
    3. El sistema genera un ID único para la tarea.
    4. La tarea se almacena en la tabla Tareas.
* **Asignación de recursos a una tarea:**
    1. El usuario selecciona la tarea a la que desea asignar recursos.
    2. El usuario selecciona los recursos que desea asignar a la tarea.
    3. El sistema actualiza la tabla Recursos para reflejar la asignación de recursos.

```
*Modelo de Interfaz de Usuario*

```
* **Pantalla principal:**
    * Lista de proyectos
    * Botones para crear, editar y eliminar proyectos
* **Pantalla de edición de proyectos:**
    * Campos para ingresar el nombre, la descripción, la fecha de inicio, la fecha de finalización y el presupuesto del proyecto
    * Botones para guardar, cancelar y eliminar el proyecto
* **Pantalla de asignación de tareas:**
    * Lista de tareas
    * Botones para crear, editar y eliminar tareas
* **Pantalla de edición de tareas:**
    * Campos para ingresar el nombre, la descripción, la fecha de inicio, la fecha de finalización, la duración, las dependencias y el estado de la tarea
    * Botones para guardar, cancelar y eliminar la tarea
* **Pantalla de asignación de recursos:**
    * Lista de recursos
    * Botones para seleccionar y deseleccionar recursos
    * Botón para guardar la asignación de recursos

```
*Diagrama de Secuencia*

```
**Creación de un proyecto:**

Usuario -> Sistema: Ingresar nombre, descripción, fecha de inicio, fecha de finalización y presupuesto del proyecto
Sistema -> Base de datos: Almacenar el proyecto en la tabla Proyectos
Sistema -> Usuario: Mostrar el proyecto en la pantalla principal

**Asignación de tareas a un proyecto:**

Usuario -> Sistema: Seleccionar el proyecto al que desea asignar tareas
Sistema -> Base de datos: Obtener la lista de tareas del proyecto
Sistema -> Usuario: Mostrar la lista de tareas en la pantalla de asignación de tareas
Usuario -> Sistema: Ingresar el nombre, la descripción, la fecha de inicio, la fecha de finalización, la duración, las dependencias y el estado de la tarea
Sistema -> Base de datos: Almacenar la tarea en la tabla Tareas
Sistema -> Usuario: Mostrar la tarea en la pantalla de asignación de tareas

**Asignación de recursos a una tarea:**

Usuario -> Sistema: Seleccionar la tarea a la que desea asignar recursos
Sistema -> Base de datos: Obtener la lista de recursos disponibles
Sistema -> Usuario: Mostrar la lista de recursos disponibles en la pantalla de asignación de recursos
Usuario -> Sistema: Seleccionar los recursos que desea asignar a la tarea
Sistema -> Base de datos: Actualizar la tabla Recursos para reflejar la asignación de recursos
Sistema -> Usuario: Mostrar la asignación de recursos en la pantalla de asignación de recursos

```
*Diagrama de Clases*

```
+----------------+
| Proyecto      |
+----------------+
| ID_Proyecto  |
| Nombre        |
| Descripción  |
| Fecha_Inicio |
| Fecha_Finalización |
| Presupuesto   |
| Estado        |
+----------------+

+----------------+
| Tarea         |
+----------------+
| ID_Tarea      |
| Nombre        |
| Descripción  |
| Fecha_Inicio |
| Fecha_Finalización |
| Duración      |
| Dependencias  |
| Estado        |
+----------------+

+----------------+
| Recurso       |
+----------------+
| ID_Recurso    |
| Nombre        |
| Tipo          |
| Disponibilidad |
| Coste         |
+----------------+

```

Este código es un ejemplo de un sistema de gestión de proyectos. Incluye un modelo de negocio, un modelo de datos, un modelo de procesos, un modelo de interfaz de usuario y un diagrama de clases. El sistema permite crear proyectos, asignar tareas a los proyectos y asignar recursos a las tareas. El sistema también permite visualizar la información de los proyectos, tareas y recursos.