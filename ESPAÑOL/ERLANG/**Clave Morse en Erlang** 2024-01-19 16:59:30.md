```erlang

-module(clave_morse).

-export([codificar/1, decodificar/1]).

% Codifica un texto en código morse.
codificar(Texto) ->
    [codificar_letra(Letra) || Letra <- Texto].

% Decodifica un código morse en texto.
decodificar(Morse) ->
    [decodificar_letra(Letra) || Letra <- Morse].

% Codifica una letra en código morse.
codificar_letra(Letra) ->
    case Letra of
        'A' -> ".- ";
        'B' -> "-... ";
        'C' -> "-.-. ";
        'D' -> "-.. ";
        'E' -> ". ";
        'F' -> "..-. ";
        'G' -> "--. ";
        'H' -> ".... ";
        'I' -> ".. ";
        'J' -> ".--- ";
        'K' -> "-.- ";
        'L' -> ".-.. ";
        'M' -> "-- ";
        'N' -> "-. ";
        'O' -> "--- ";
        'P' -> ".--. ";
        'Q' -> "--.- ";
        'R' -> ".-. ";
        'S' -> "... ";
        'T' -> "- ";
        'U' -> "..- ";
        'V' -> "...- ";
        'W' -> ".-- ";
        'X' -> "-..- ";
        'Y' -> "-.-- ";
        'Z' -> "--.. ";
        '.' -> ".---- ";
        ',' -> "--..-- ";
        '?' -> "..--.. ";
        '_' -> "..--.- ";
        _ -> ""
    end.

% Decodifica una letra en código morse.
decodificar_letra(Letra) ->
    case Letra of
        ".- " -> 'A';
        "-... " -> 'B';
        "-.-. " -> 'C';
        "-.. " -> 'D';
        ". " -> 'E';
        "..-. " -> 'F';
        "--. " -> 'G';
        ".... " -> 'H';
        ".. " -> 'I';
        ".--- " -> 'J';
        "-.-" -> 'K';
        ".-.. " -> 'L';
        "-- " -> 'M';
        "-. " -> 'N';
        "--- " -> 'O';
        ".--. " -> 'P';
        "--.- " -> 'Q';
        ".-. " -> 'R';
        "..." -> 'S';
        "-" -> 'T';
        "..- " -> 'U';
        "...- " -> 'V';
        ".-- " -> 'W';
        "-..- " -> 'X';
        "-.--" -> 'Y';
        "--.. " -> 'Z';
        ".---- " -> '.';
        "--..-- " -> ',';
        "..--.. " -> '?';
        "..--.- " -> '_';
        _ -> ""
    end.

```

Explicación:

El código que has proporcionado es una implementación del código Morse en Erlang. Consta de dos funciones principales, codificar y decodificar, que se utilizan para codificar y decodificar texto en código Morse, respectivamente.

La función codificar toma una lista de caracteres como argumento y devuelve una lista de códigos Morse correspondientes. Utiliza un case para comprobar cada carácter y devuelve el código Morse correspondiente.

La función decodificar toma una lista de códigos Morse como argumento y devuelve una lista de caracteres correspondientes. Utiliza un case para comprobar cada código Morse y devuelve el carácter correspondiente.

El código también define dos funciones auxiliares, codificar_letra y decodificar_letra, que se utilizan para codificar y decodificar un solo carácter, respectivamente. Estas funciones se utilizan en las funciones codificar y decodificar.

El código Morse es un código de transmisión de información que utiliza diferentes patrones de puntos y rayas para representar letras, números y signos de puntuación. Se utiliza en una variedad de aplicaciones, como la comunicación por radio y telégrafo.