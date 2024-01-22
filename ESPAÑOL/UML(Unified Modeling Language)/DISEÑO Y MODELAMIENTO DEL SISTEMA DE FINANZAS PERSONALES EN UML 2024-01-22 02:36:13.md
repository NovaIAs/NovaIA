```
*******************************************************************************************************
*                                   DIAGRAMA DE CASOS DE USO                                    *
*******************************************************************************************************

* Actores:
    * Usuario: Persona que utiliza el sistema para gestionar sus finanzas personales.
    * Administrador: Persona que tiene permisos para gestionar las cuentas de usuario y los datos del sistema.

* Casos de uso:
    * Autenticación: El usuario se identifica en el sistema.
    * Registro: El usuario crea una cuenta en el sistema.
    * Gestión de ingresos: El usuario registra sus ingresos.
    * Gestión de gastos: El usuario registra sus gastos.
    * Gestión de categorías: El usuario crea y gestiona las categorías de sus ingresos y gastos.
    * Gestión de cuentas: El usuario crea y gestiona sus cuentas bancarias y de inversión.
    * Gestión de objetivos financieros: El usuario crea y gestiona sus objetivos financieros.
    * Generación de informes: El usuario genera informes sobre sus finanzas personales.
    * Configuración del sistema: El administrador configura el sistema.
    * Gestión de usuarios: El administrador gestiona las cuentas de usuario.
    * Gestión de datos: El administrador gestiona los datos del sistema.

*******************************************************************************************************
*                                   DIAGRAMA DE CLASES                                       *
*******************************************************************************************************

* Clases:
    * Usuario: Representa a un usuario del sistema.
    * Administrador: Representa a un administrador del sistema.
    * Ingreso: Representa un ingreso del usuario.
    * Gasto: Representa un gasto del usuario.
    * Categoría: Representa una categoría de ingresos o gastos.
    * Cuenta: Representa una cuenta bancaria o de inversión del usuario.
    * Objetivo financiero: Representa un objetivo financiero del usuario.
    * Informe: Representa un informe sobre las finanzas personales del usuario.
    * Configuración del sistema: Representa la configuración del sistema.

*******************************************************************************************************
*                                  DIAGRAMA DE SECUENCIA                                   *
*******************************************************************************************************

* Caso de uso: Gestión de ingresos

1. El usuario hace clic en el botón "Nuevo ingreso" en la interfaz de usuario.
2. El sistema muestra un formulario para introducir los datos del ingreso.
3. El usuario introduce los datos del ingreso y hace clic en el botón "Guardar".
4. El sistema guarda el ingreso en la base de datos.
5. El sistema muestra el ingreso en la lista de ingresos del usuario.

*******************************************************************************************************
*                                  DIAGRAMA DE ACTIVIDADES                                    *
*******************************************************************************************************

* Caso de uso: Generación de informes

1. El usuario hace clic en el botón "Generar informe" en la interfaz de usuario.
2. El sistema muestra un formulario para seleccionar los datos del informe.
3. El usuario selecciona los datos del informe y hace clic en el botón "Generar".
4. El sistema genera el informe y lo muestra en la pantalla.
5. El usuario puede guardar el informe o imprimirlo.

*******************************************************************************************************
*                                  DIAGRAMA DE COMPOSICIÓN                                   *
*******************************************************************************************************

* Clase: Usuario

- Atributos:
    * Nombre
    * Apellidos
    * Correo electrónico
    * Contraseña
    * Fecha de nacimiento
    * Género

- Métodos:
    * Registrarse()
    * Iniciar sesión()
    * Cerrar sesión()
    * Obtener ingresos()
    * Obtener gastos()
    * Obtener categorías()
    * Obtener cuentas()
    * Obtener objetivos financieros()
    * Generar informes()

*******************************************************************************************************
*                                  DIAGRAMA DE COMUNICACIÓN                                     *
*******************************************************************************************************

* Objetos:
    * Usuario
    * Administrador
    * Ingreso
    * Gasto
    * Categoría
    * Cuenta
    * Objetivo financiero
    * Informe
    * Configuración del sistema

* Mensajes:
    * Registrarse: El usuario envía un mensaje al sistema para registrarse.
    * Iniciar sesión: El usuario envía un mensaje al sistema para iniciar sesión.
    * Cerrar sesión: El usuario envía un mensaje al sistema para cerrar sesión.
    * Obtener ingresos: El usuario envía un mensaje al sistema para obtener sus ingresos.
    * Obtener gastos: El usuario envía un mensaje al sistema para obtener sus gastos.
    * Obtener categorías: El usuario envía un mensaje al sistema para obtener sus categorías.
    * Obtener cuentas: El usuario envía un mensaje al sistema para obtener sus cuentas.
    * Obtener objetivos financieros: El usuario envía un mensaje al sistema para obtener sus objetivos financieros.
    * Generar informes: El usuario envía un mensaje al sistema para generar informes.
    * Configurar el sistema: El administrador envía un mensaje al sistema para configurar el sistema.
    * Gestionar usuarios: El administrador envía un mensaje al sistema para gestionar las cuentas de usuario.
    * Gestionar datos: El administrador envía un mensaje al sistema para gestionar los datos del sistema.

*******************************************************************************************************
*                                    DIAGRAMA DE IMPLEMENTACIÓN                                 *
********************************