```erlang
% Definimos un módulo llamado "mi_modulo".
-module(mi_modulo).

% Exportamos la función "hola_mundo" para que pueda ser utilizada desde otros módulos.
-export([hola_mundo/0]).

% Definimos la función "hola_mundo".
hola_mundo() ->
    io:fwrite("¡Hola, mundo!").

% Definimos un módulo llamado "mi_otro_modulo".
-module(mi_otro_modulo).

% Exportamos la función "adios_mundo" para que pueda ser utilizada desde otros módulos.
-export([adios_mundo/0]).

% Definimos la función "adios_mundo".
adios_mundo() ->
    io:fwrite("Adiós, mundo!").

% Definimos un módulo llamado "mi_principal_modulo".
-module(mi_principal_modulo).

% Importamos el módulo "mi_modulo".
-import(mi_modulo, [hola_mundo/0]).

% Importamos el módulo "mi_otro_modulo".
-import(mi_otro_modulo, [adios_mundo/0]).

% Definimos la función principal.
-export([main/0]).

% Definimos la función principal.
main() ->
    hola_mundo(),
    adios_mundo().

```

Explicación:

* El primer módulo, `mi_modulo`, define una función llamada `hola_mundo()` que imprime el mensaje "Hola, mundo!" en la consola.
* El segundo módulo, `mi_otro_modulo`, define una función llamada `adios_mundo()` que imprime el mensaje "Adiós, mundo!" en la consola.
* El tercer módulo, `mi_principal_modulo`, importa las funciones `hola_mundo()` y `adios_mundo()` de los módulos `mi_modulo` y `mi_otro_modulo`, respectivamente.
* La función principal, `main()`, llama a las funciones `hola_mundo()` y `adios_mundo()`, que imprimen los mensajes "Hola, mundo!" y "Adiós, mundo!" en la consola, respectivamente.

Este código es un ejemplo sencillo de cómo utilizar módulos y funciones en Erlang. Puede utilizarse para crear programas más complejos dividiendo el código en módulos separados, lo que facilita su mantenimiento y reutilización.