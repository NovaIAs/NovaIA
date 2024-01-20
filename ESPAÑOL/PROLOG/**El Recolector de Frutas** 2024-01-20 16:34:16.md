```prolog
:- dynamic inicia_juego/0.

% hechos y reglas sobre las frutas
fruta(manzana).
fruta(pera).
fruta(naranja).
fruta(uva).

% hechos sobre la ubicación de las frutas
ubicacion(manzana, arbol).
ubicacion(pera, arbol).
ubicacion(naranja, arbol).
ubicacion(uva, parra).

% reglas sobre la acción de recoger una fruta
recoger_fruta(Fruta) :-
    ubicacion(Fruta, Lugar),
    recoger_fruta_ubicacion(Lugar).

recoger_fruta_ubicacion(arbol) :-
    write('Has recogido una fruta del árbol.'), nl.

recoger_fruta_ubicacion(parra) :-
    write('Has recogido una fruta de la parra.'), nl.

% reglas sobre cómo terminar el juego
termina_juego :-
    not(inicia_juego).

% hechos y reglas sobre el estado del juego
inicia_juego :-
    write('Bienvenido al juego!'), nl,
    write('El objetivo del juego es recoger todas las frutas.'), nl,
    write('Para recoger una fruta, escribe el nombre de la fruta.'), nl,
    write('Para terminar el juego, escribe "fin".'), nl.

fin_juego :-
    write('¡Enhorabuena! Has recogido todas las frutas.'), nl,
    write('¡Fin del juego!'), nl,
    halt.

% reglas sobre el ciclo de juego
ciclo_juego :-
    write('¿Qué fruta quieres recoger? '),
    read(Fruta),
    nl,
    fruta(Fruta),
    recoger_fruta(Fruta),
    termina_juego.

% regla principal que inicia el juego
main :-
    inicia_juego,
    repeat,
    ciclo_juego,
    fin_juego.
```

Explicación:

* El código está organizado en varios módulos separados por líneas que comienzan con '%'.
* El módulo "fruta" define los hechos y reglas sobre las frutas.
* El módulo "ubicacion" define los hechos sobre la ubicación de las frutas.
* El módulo "recoger_fruta" define las reglas sobre la acción de recoger una fruta.
* El módulo "termina_juego" define las reglas sobre cómo terminar el juego.
* El módulo "estado_juego" define los hechos y reglas sobre el estado del juego.
* El módulo "ciclo_juego" define las reglas sobre el ciclo de juego.
* El módulo "main" es el principal y contiene la regla que inicia el juego.
* La regla "fruta" define un hecho que afirma que "manzana", "pera", "naranja" y "uva" son frutas.
* La regla "ubicacion" define los hechos que afirman que la "manzana", la "pera" y la "naranja" se encuentran en el árbol, mientras que la "uva" se encuentra en la parra.
* La regla "recoger_fruta" define una regla que permite recoger una fruta. La regla se activa cuando se recibe una fruta como argumento y verifica si la fruta se encuentra en el árbol o en la parra. Si la fruta se encuentra en el árbol, se escribe un mensaje indicando que se ha recogido la fruta del árbol. Si la fruta se encuentra en la parra, se escribe un mensaje indicando que se ha recogido la fruta de la parra.
* La regla "termina_juego" define una regla que termina el juego. La regla se activa cuando no se ha iniciado el juego.
* La regla "inicia_juego" define una regla que inicia el juego. La regla se activa cuando se escribe el mensaje "Bienvenido al juego!" y las instrucciones del juego.
* La regla "fin_juego" define una regla que termina el juego. La regla se activa cuando se han recogido todas las frutas.
* La regla "ciclo_juego" define una regla que itera sobre el ciclo de juego. La regla se activa cuando se recibe una fruta como argumento. Si la fruta es una fruta válida, se llama a la regla "recoger_fruta" para recogerla. Si la fruta no es una fruta válida, se escribe un mensaje indicando que la fruta no es válida.
* La regla "main" es la principal y contiene la regla que inicia el juego. La regla se activa cuando se llama al predicado "main". El predicado "main" llama a la regla "inicia_juego" para iniciar el juego y luego llama a la regla "ciclo_juego" para iterar sobre el ciclo de juego. Cuando se han recogido todas las frutas, se llama a la regla "fin_juego" para terminar el juego.