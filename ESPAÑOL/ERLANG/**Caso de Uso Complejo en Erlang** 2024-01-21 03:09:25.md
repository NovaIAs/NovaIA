```erlang
-module(caso_de_uso_complejo).
-export([funcion_principal/0, funcion_secundaria/1, funcion_terciaria/2]).

funcion_principal() ->
    % Llamamos a la función secundaria con el valor "Hola"
    Resultado = funcion_secundaria("Hola"),
    % Imprimimos el resultado de la función secundaria
    io:format("El resultado de la función secundaria es: ~p~n", [Resultado]),

    % Llamamos a la función terciaria con los valores "Mundo" y "!"
    ResultadoFinal = funcion_terciaria("Mundo", "!"),
    % Imprimimos el resultado de la función terciaria
    io:format("El resultado final es: ~p~n", [ResultadoFinal]).

funcion_secundaria(Valor) ->
    % Concatenamos el valor recibido con la cadena " Mundo"
    Resultado = Valor ++ " Mundo",
    % Devolvemos el resultado
    Resultado.

funcion_terciaria(Valor1, Valor2) ->
    % Concatenamos los dos valores recibidos
    Resultado = Valor1 ++ Valor2,
    % Convertimos el resultado a mayúsculas
    ResultadoFinal = string:to_upper(Resultado),
    % Devolvemos el resultado final
    ResultadoFinal.
```

Explicación:

* El módulo `caso_de_uso_complejo` define tres funciones: `funcion_principal`, `funcion_secundaria` y `funcion_terciaria`.
* La función `funcion_principal` llama a la función `funcion_secundaria` con el valor "Hola" y luego llama a la función `funcion_terciaria` con los valores "Mundo" y "!". Finalmente, imprime el resultado de la función `funcion_terciaria`.
* La función `funcion_secundaria` recibe un valor y lo concatena con la cadena " Mundo". Luego, devuelve el resultado.
* La función `funcion_terciaria` recibe dos valores y los concatena. Luego, convierte el resultado a mayúsculas y lo devuelve.

Este código es complejo porque utiliza varias funciones y llamadas a funciones, así como concatenación de cadenas y conversión a mayúsculas. También utiliza el módulo `io` para imprimir el resultado de las funciones.