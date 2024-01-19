```
**Diagrama de Clases**

**Clase Sistema**

* Atributos:
    * Nombre
    * Descripción
    * Fecha de creación
    * Fecha de modificación
* Métodos:
    * Crear usuario
    * Modificar usuario
    * Eliminar usuario
    * Crear grupo
    * Modificar grupo
    * Eliminar grupo
    * Crear recurso
    * Modificar recurso
    * Eliminar recurso
    * Asignar permiso
    * Quitar permiso
    * Iniciar sesión
    * Cerrar sesión

**Clase Usuario**

* Atributos:
    * Nombre
    * Apellidos
    * Correo electrónico
    * Contraseña
    * Fecha de nacimiento
    * Género
    * Número de teléfono
* Métodos:
    * Cambiar contraseña
    * Actualizar información personal
    * Crear grupo
    * Modificar grupo
    * Eliminar grupo
    * Crear recurso
    * Modificar recurso
    * Eliminar recurso
    * Asignar permiso
    * Quitar permiso
    * Iniciar sesión
    * Cerrar sesión

**Clase Grupo**

* Atributos:
    * Nombre
    * Descripción
    * Fecha de creación
    * Fecha de modificación
* Métodos:
    * Agregar usuario
    * Eliminar usuario
    * Asignar permiso
    * Quitar permiso

**Clase Recurso**

* Atributos:
    * Nombre
    * Descripción
    * Tipo
    * Fecha de creación
    * Fecha de modificación
* Métodos:
    * Crear versión
    * Modificar versión
    * Eliminar versión
    * Asignar permiso
    * Quitar permiso

**Clase Permiso**

* Atributos:
    * Nombre
    * Descripción
    * Tipo
* Métodos:
    * Asignar a usuario
    * Asignar a grupo
    * Quitar de usuario
    * Quitar de grupo

**Diagrama de Secuencia**

**Caso de uso: Iniciar sesión**

1. El usuario ingresa su correo electrónico y contraseña.
2. El sistema verifica las credenciales del usuario.
3. Si las credenciales son correctas, el sistema inicia la sesión del usuario.
4. Si las credenciales son incorrectas, el sistema muestra un mensaje de error.

**Diagrama de Estados**

**Clase Usuario**

* Estados:
    * Inactivo
    * Activo
    * Bloqueado
* Transiciones:
    * Inactivo -> Activo: cuando el usuario se registra en el sistema.
    * Activo -> Bloqueado: cuando el usuario ingresa credenciales incorrectas varias veces.
    * Bloqueado -> Activo: cuando el usuario se desbloquea.

**Diagrama de Actividades**

**Caso de uso: Crear un recurso**

1. El usuario selecciona el tipo de recurso que desea crear.
2. El usuario ingresa la información requerida para crear el recurso.
3. El sistema verifica que la información ingresada sea válida.
4. Si la información es válida, el sistema crea el recurso.
5. Si la información no es válida, el sistema muestra un mensaje de error.

**Explicación del código**

El código anterior es un ejemplo de cómo utilizar UML para modelar un sistema de gestión de usuarios y recursos. El diagrama de clases define las clases del sistema y sus relaciones. El diagrama de secuencia define un caso de uso y los pasos que se siguen para llevarlo a cabo. El diagrama de estados define los estados de una clase y las transiciones entre ellos. El diagrama de actividades define un caso de uso y las actividades que se llevan a cabo para llevarlo a cabo.

Este código es sólo un ejemplo, y la forma en que se modela un sistema puede variar dependiendo de los requisitos del sistema y de las preferencias del modelador.
```