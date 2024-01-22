```
+---------------------------------------------------------------------------------------------------------+
| Use Case: Gestión de Clientes                                        |
+---------------------------------------------------------------------------------------------------------+

+---------------------------------------------------------------------+
| Actor: Administrador de Clientes                                    |
+---------------------------------------------------------------------+

+---------------------------------------------------------------------------------------------------------+
| Secuencia de Eventos:                                                            |
+---------------------------------------------------------------------------------------------------------+
| 1. El Administrador de Clientes inicia la aplicación de Gestión de Clientes. |
| 2. La aplicación muestra el formulario de inicio de sesión.                     |
| 3. El Administrador de Clientes introduce su nombre de usuario y contraseña.  |
| 4. La aplicación valida las credenciales y, si son correctas, inicia la sesión. |
| 5. La aplicación muestra el menú principal de la aplicación.                      |
| 6. El Administrador de Clientes selecciona la opción "Gestionar clientes".      |
| 7. La aplicación muestra la lista de clientes.                                |
| 8. El Administrador de Clientes selecciona un cliente de la lista.            |
| 9. La aplicación muestra el detalle del cliente seleccionado.                  |
| 10. El Administrador de Clientes puede realizar las siguientes acciones:       |
|      * Editar el detalle del cliente.                                       |
|      * Eliminar el cliente.                                                 |
| 11. El Administrador de Clientes selecciona una acción y confirma su elección. |
| 12. La aplicación realiza la acción seleccionada.                         |
| 13. La aplicación muestra un mensaje de confirmación de la acción realizada.  |
| 14. El Administrador de Clientes puede repetir los pasos 7 a 13 para gestionar otros clientes. |
| 15. El Administrador de Clientes puede cerrar la sesión seleccionando la opción "Cerrar sesión" en el menú principal. |
+---------------------------------------------------------------------------------------------------------+

+---------------------------------------------------------------------------------------------------------+
| Diagrama de Clases:                                                                                  |
+---------------------------------------------------------------------------------------------------------+

+-----------------------------------------------------------------------------------------------------+
| Clase: Aplicación de Gestión de Clientes                                                           |
+-----------------------------------------------------------------------------------------------------+
| Atributos:                                                                                           |
|      * Nombre de usuario                                                                             |
|      * Contraseña                                                                                   |
|      * Lista de clientes                                                                              |
|      * Menú principal                                                                               |
| Métodos:                                                                                             |
|      * Iniciar sesión                                                                                 |
|      * Cerrar sesión                                                                                |
|      * Gestionar clientes                                                                            |
|      * Mostrar la lista de clientes                                                                   |
|      * Mostrar el detalle del cliente seleccionado                                                     |
|      * Editar el detalle del cliente                                                                 |
|      * Eliminar el cliente                                                                           |
|      * Mostrar un mensaje de confirmación de la acción realizada                                    |
+-----------------------------------------------------------------------------------------------------+

+----------------------------------------------------------------------------------------------------+
| Clase: Administrador de Clientes                                                                    |
+----------------------------------------------------------------------------------------------------+
| Atributos:                                                                                              |
|      * Nombre de usuario                                                                               |
|      * Contraseña                                                                                       |
| Métodos:                                                                                                 |
|      * Iniciar sesión                                                                                     |
|      * Cerrar sesión                                                                                      |
|      * Gestionar clientes                                                                                  |
|      * Mostrar la lista de clientes                                                                         |
|      * Mostrar el detalle del cliente seleccionado                                                           |
|      * Editar el detalle del cliente                                                                     |
|      * Eliminar el cliente                                                                                 |
|      * Mostrar un mensaje de confirmación de la acción realizada                                        |
+----------------------------------------------------------------------------------------------------+

+---------------------------------------------------------------------------------------------------------+
| Clase: Cliente                                                                                           |
+---------------------------------------------------------------------------------------------------------+
| Atributos:                                                                                                |
|      * Nombre                                                                                              |
|      * Apellidos                                                                                            |
|      * Dirección                                                                                              |
|      * Teléfono                                                                                              |
|      * Correo electrónico                                                                                    |
+---------------------------------------------------------------------------------------------------------+

+---------------------------------------------------------------------------------------------------------+
| Clase: Lista de Clientes                                                                                  |
+---------------------------------------------------------------------------------------------------------+
| Atributos:                                                                                                |
|      * Lista de clientes                                                                                    |
| Métodos:                                                                                                   |
|      * Añadir cliente                                                                                         |
|      * Eliminar cliente                                                                                        |
|      * Buscar cliente                                                                                          |
|      * Mostrar lista de clientes                                                                             |
+---------------------------------------------------------------------------------------------------------+

+---------------------------------------------------------------------------------------------------------+
| Clase: Menú Principal                                                                                        |
+---------------------------------------------------------------------------------------------------------+
| Atributos:                                                                                                |
|      * Opciones del menú                                                                                     |
| Métodos:                                                                                                   |
|      * Mostrar menú principal                                                                                  |
|      * Ejecutar opción seleccionada                                                                             |
+---------------------------------------------------------------------------------------------------------+
```

Explicación del código:

* **Use Case:** Gestionar Clientes: Este es el caso de uso principal de la aplicación, que define el flujo de eventos y los actores involucrados en la gestión de clientes.


* **Actor:** Administrador de Clientes: Es el actor principal del caso de uso, que es responsable de gestionar a los clientes de la aplicación.


* **Secuencia de Eventos:** Define el flujo de eventos que ocurren durante la gestión de clientes, incluyendo el inicio de sesión, la selección de un cliente, el detalle del cliente, la edición del detalle del cliente, la eliminación del cliente, la confirmación de las acciones y la repetición del proceso para otros clientes.


* **Diagrama de Clases:** Representa las clases involucradas en la gestión de clientes, incluyendo la aplicación de gestión de clientes, el administrador de clientes, el cliente, la lista de clientes, el menú principal y las posibles acciones.


* **Clase Aplicación de Gestión de Clientes:** Representa la aplicación que gestiona a los clientes, incluyendo los atributos (nombre de usuario, contraseña, lista de clientes, menú principal) y los métodos (iniciar sesión, cerrar sesión, gestionar clientes, mostrar la lista de clientes, mostrar el detalle del cliente seleccionado, editar el detalle del cliente, eliminar el cliente, mostrar un mensaje de confirmación de la acción realizada).


* **Clase Administrador de Clientes:** Representa al administrador que gestiona a los clientes, incluyendo los atributos (nombre de usuario, contraseña) y los métodos (iniciar sesión, cerrar sesión, gestionar clientes, mostrar la lista de clientes, mostrar el detalle del cliente seleccionado, editar el detalle del cliente, eliminar el cliente, mostrar un mensaje de confirmación de la acción realizada).


* **Clase Cliente:** Representa a un cliente gestionado por la aplicación, incluyendo los atributos (nombre, apellidos, dirección, teléfono, correo electrónico).


* **Clase Lista de Clientes:** Representa la lista de clientes gestionada por la aplicación, incluyendo los métodos (añadir cliente, eliminar cliente, buscar cliente, mostrar lista de clientes).


* **Clase Menú Principal:** Representa el menú principal de la aplicación, incluyendo los métodos (mostrar menú principal, ejecutar opción seleccionada).