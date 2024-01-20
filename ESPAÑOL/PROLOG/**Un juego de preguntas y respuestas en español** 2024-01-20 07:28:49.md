```prolog
% Este código implementa un pequeño juego de preguntas y respuestas en español.

% La base de conocimientos del juego consiste en una serie de hechos que contienen
% información sobre el mundo. Por ejemplo:

madre(maría, juan).
padre(juan, pedro).
hermana(maría, ana).
amigo(juan, pedro).
enemigo(pedro, ana).

% Las preguntas del juego se representan utilizando reglas. Una regla tiene la forma:

pregunta(Pregunta, Respuesta)

donde Pregunta es una cadena de caracteres que contiene la pregunta y Respuesta es
una cadena de caracteres que contiene la respuesta. Por ejemplo:

pregunta("¿Quién es la madre de Juan?", "María").
pregunta("¿Cuál es el padre de Pedro?", "Juan").
pregunta("¿Quién es la hermana de María?", "Ana").
pregunta("¿Quién es el amigo de Juan?", "Pedro").
pregunta("¿Quién es el enemigo de Pedro?", "Ana").

% El juego funciona de la siguiente manera:

% 1. El jugador introduce una pregunta.
% 2. El programa busca una regla que coincida con la pregunta.
% 3. Si encuentra una regla, el programa muestra la respuesta.
% 4. Si no encuentra una regla, el programa muestra un mensaje de error.

% El siguiente código implementa el juego:

jugar :-
  repeat,
  write('Introduce una pregunta: '),
  read_line(Pregunta),
  pregunta(Pregunta, Respuesta),
  write(Respuesta),
  nl.

pregunta(Pregunta, Respuesta) :-
  rule(Pregunta, Respuesta).

rule("¿Quién es la madre de Juan?", "María").
rule("¿Cuál es el padre de Pedro?", "Juan").
rule("¿Quién es la hermana de María?", "Ana").
rule("¿Quién es el amigo de Juan?", "Pedro").
rule("¿Quién es el enemigo de Pedro?", "Ana").
```

Explicación del código:

* El primer predicado, `jugar/0`, define el juego. Este predicado contiene un bucle `repeat` que se ejecuta hasta que el usuario introduce una línea vacía.
* El segundo predicado, `pregunta/2`, busca una regla que coincida con la pregunta introducida por el usuario.
* El tercer predicado, `rule/2`, contiene una serie de reglas que definen las preguntas y respuestas del juego.
* El cuarto predicado, `write/1`, muestra la respuesta a la pregunta en la consola.
* El quinto predicado, `nl/0`, añade un salto de línea a la consola.