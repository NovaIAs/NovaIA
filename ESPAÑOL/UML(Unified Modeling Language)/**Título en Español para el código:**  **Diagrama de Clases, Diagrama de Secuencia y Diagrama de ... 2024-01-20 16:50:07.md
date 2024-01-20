**Diagrama de Clases**

```
+----------------+
| Clase Persona |
+----------------+
| - Nombre      |
| - Apellidos   |
| - FechaNacimiento |
| - Genero      |
| - Direccion   |
| - Telefono    |
| - Email       |
+----------------+

+----------------+
| Clase Direccion |
+----------------+
| - Calle      |
| - Numero     |
| - Piso       |
| - Puerta     |
| - CodigoPostal |
| - Poblacion  |
| - Provincia  |
| - Pais       |
+----------------+

+----------------+
| Clase Telefono |
+----------------+
| - NumeroTelefono |
| - TipoTelefono  |
+----------------+

+----------------+
| Clase Email    |
+----------------+
| - Email        |
| - TipoEmail    |
+----------------+
```

**Diagrama de Secuencia**

```
1. El usuario introduce sus datos personales en el formulario.
2. El formulario envía los datos a la base de datos.
3. La base de datos guarda los datos en la tabla de personas.
4. El usuario es redirigido a la página de confirmación.
5. El usuario puede ver sus datos personales en la página de confirmación.
6. El usuario puede modificar sus datos personales en la página de modificación.
7. El usuario puede eliminar sus datos personales de la base de datos.
```

**Diagrama de Casos de Uso**

```
+--------------------------------+
| Caso de Uso: Crear una persona |
+--------------------------------+
| Actores: Usuario                 |
| Descripción: El usuario introduce |
| sus datos personales en el       |
| formulario.                    |
+--------------------------------+

+------------------------------------+
| Caso de Uso: Ver una persona      |
+------------------------------------+
| Actores: Usuario                 |
| Descripción: El usuario puede ver |
| sus datos personales en la página |
| de confirmación.                  |
+------------------------------------+

+----------------------------------------+
| Caso de Uso: Modificar una persona     |
+----------------------------------------+
| Actores: Usuario                       |
| Descripción: El usuario puede modificar |
| sus datos personales en la página de   |
| modificación.                          |
+----------------------------------------+

+----------------------------------------+
| Caso de Uso: Eliminar una persona     |
+----------------------------------------+
| Actores: Usuario                       |
| Descripción: El usuario puede eliminar |
| sus datos personales de la base de     |
| datos.                                |
+----------------------------------------+
```

**Explicación del código**

El código UML anterior describe un sistema de gestión de personas. El sistema permite al usuario crear, ver, modificar y eliminar personas. El sistema también permite al usuario ver una lista de todas las personas en el sistema.

El diagrama de clases describe las clases del sistema. La clase Persona representa una persona. La clase Dirección representa una dirección. La clase Teléfono representa un número de teléfono. La clase Email representa una dirección de correo electrónico.

El diagrama de secuencia describe el flujo de eventos del sistema. El diagrama de casos de uso describe los casos de uso del sistema.