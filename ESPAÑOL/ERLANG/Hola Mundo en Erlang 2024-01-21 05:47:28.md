```erlang
-module(hola_mundo).
-export([hola_mundo/0]).

hola_mundo() ->
    io:fwrite("¡Hola, mundo! ~n").
```

Este código define un módulo llamado `hola_mundo` con una función `hola_mundo`. Cuando se llama a la función `hola_mundo`, imprime la frase "¡Hola, mundo!" en la consola.

El código utiliza la función `io:fwrite` para imprimir la frase. La función `io:fwrite` toma dos argumentos: el primer argumento es la frase que se va a imprimir y el segundo argumento es el dispositivo de salida. En este caso, el dispositivo de salida es `~n`, que representa el final de la línea.

El código también utiliza el operador `->` para definir la función `hola_mundo`. El operador `->` se utiliza para definir funciones en Erlang. El código después del operador `->` define el cuerpo de la función.

En este caso, el cuerpo de la función es una llamada a la función `io:fwrite`. La función `io:fwrite` imprime la frase "¡Hola, mundo!" en la consola.

El código también utiliza el operador `-export` para exportar la función `hola_mundo`. El operador `-export` se utiliza para exportar funciones desde un módulo. Esto permite que otras partes del programa llamen a la función `hola_mundo`.

En este caso, la función `hola_mundo` se exporta para que pueda ser llamada desde otros módulos. Esto permite que otros módulos impriman la frase "¡Hola, mundo!" en la consola.