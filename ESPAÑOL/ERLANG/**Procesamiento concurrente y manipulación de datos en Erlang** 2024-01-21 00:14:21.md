```erlang
% Definición de los módulos.
-module(modulo_principal).
-module(modulo_secundario).

% Exportación de las funciones.
-export([funcion_principal/0, funcion_secundaria/0]).

% Función principal.
funcion_principal() ->
  % Llamada a la función secundaria.
  modulo_secundario:funcion_secundaria().

% Función secundaria.
funcion_secundaria() ->
  % Devuelve un valor.
  42.

% Definición de una estructura de datos.
-record(persona, {nombre, edad}).

% Creación de una estructura de datos.
persona = #persona{nombre = "Juan", edad = 25}.

% Coincidencia de patrones.
case persona of
  #persona{nombre = "Juan"} -> % Si el nombre es "Juan"
    io:format("El nombre es Juan.~n");
  #persona{edad = 25} -> % Si la edad es 25
    io:format("La edad es 25.~n");
  _ -> % Si no coincide con ninguno de los casos anteriores
    io:format("No se encontraron coincidencias.~n")
end.

% Generación de listas.
lista = [1, 2, 3, 4, 5].

% Iteración sobre listas.
lists:foreach(fun(X) -> io:format("~w~n", [X]) end, lista).

% Procesos.
spawn(fun() -> io:format("Hola desde un proceso.~n") end).

% Bucle infinito.
loop() ->
  io:format("Hola desde un bucle infinito.~n"),
  loop().

% Registro de eventos.
erlang:event(info, {mensaje, "Hola desde un evento."}).

% Manejador de eventos.
erlang:register_event_handler(info, fun({mensaje, Mensaje}) ->
  io:format("~s~n", [Mensaje])
end).

% Concurrencia.
spawn(fun() -> loop() end).
spawn(fun() -> loop() end).
```

Explicación del código:

* El código define dos módulos, `modulo_principal` y `modulo_secundario`, y exporta dos funciones, `funcion_principal/0` y `funcion_secundaria/0`.


* La función `funcion_principal/0` llama a la función `funcion_secundaria/0` del módulo `modulo_secundario`.


* La función `funcion_secundaria/0` devuelve un valor.


* El código define una estructura de datos llamada `persona` y crea una instancia de la misma.


* El código utiliza coincidencia de patrones para comprobar si el nombre o la edad de la persona son iguales a ciertos valores.


* El código genera una lista y luego itera sobre ella utilizando la función `lists:foreach/2`.


* El código crea un proceso utilizando la función `spawn/1` que escribe "Hola desde un proceso." en la consola.


* El código define una función llamada `loop/0` que escribe "Hola desde un bucle infinito." en la consola y luego se llama a sí misma de forma recursiva, creando así un bucle infinito.


* El código registra un manejador de eventos que imprime el mensaje recibido en la consola.


* El código crea dos procesos que ejecutan la función `loop/0`, lo que crea dos bucles infinitos que se ejecutan concurrentemente.