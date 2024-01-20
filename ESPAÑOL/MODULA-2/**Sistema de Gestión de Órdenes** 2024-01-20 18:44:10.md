```modula-2
MODULE SistemaGestion;

FROM Terminal IMPORT
    LeerTecla, EscribirCadena;

FROM Ordenes IMPORT
    ValidarOrden, RealizarOrden;

FROM Datos IMPORT
    ObtenerDatos, GuardarDatos;

VAR
    orden: STRING;
BEGIN
    REPEAT
        EscribirCadena("Introduzca una orden:");
        LeerTecla(orden);
        IF ValidarOrden(orden) THEN
            RealizarOrden(orden);
        ELSE
            EscribirCadena("Orden no válida.");
        FI;
    UNTIL orden = "salir";

    GuardarDatos();
END SistemaGestion.
```

Este código en MODULA-2 crea un sistema de gestión de órdenes simple que permite al usuario introducir órdenes a través de la terminal. El sistema valida las órdenes introducidas y las realiza si son válidas. También guarda los datos en un fichero.

El código se compone de cuatro módulos:

* `Terminal`: Este módulo proporciona las funciones para leer y escribir datos en la terminal.
* `Ordenes`: Este módulo proporciona las funciones para validar y realizar órdenes.
* `Datos`: Este módulo proporciona las funciones para obtener y guardar datos.
* `SistemaGestion`: Este módulo es el módulo principal del sistema.

El módulo `SistemaGestion` contiene la función `main`, que es el punto de entrada del programa. La función `main` crea un bucle `REPEAT` que se ejecuta hasta que el usuario introduce la orden "salir". Dentro del bucle, la función `main` solicita al usuario que introduzca una orden, valida la orden y la realiza si es válida.

El módulo `Terminal` contiene las funciones `LeerTecla` y `EscribirCadena`. La función `LeerTecla` lee una tecla de la terminal y la devuelve como una cadena de caracteres. La función `EscribirCadena` escribe una cadena de caracteres en la terminal.

El módulo `Ordenes` contiene las funciones `ValidarOrden` y `RealizarOrden`. La función `ValidarOrden` comprueba si una orden es válida. La función `RealizarOrden` realiza una orden si es válida.

El módulo `Datos` contiene las funciones `ObtenerDatos` y `GuardarDatos`. La función `ObtenerDatos` obtiene los datos del fichero de datos. La función `GuardarDatos` guarda los datos en el fichero de datos.