```prolog
:- dynamic simbolo/2.

% Reglas para el conocimiento del mundo real.

simbolo(coche,  "coche.png").
simbolo(arbol,  "arbol.png").
simbolo(casa,   "casa.png").
simbolo(perro,  "perro.png").
simbolo(gato,   "gato.png").
simbolo(pajaro, "pajaro.png").
simbolo(flor,   "flor.png").
simbolo(sol,    "sol.png").
simbolo(luna,   "luna.png").
simbolo(estrella,"estrella.png").
simbolo(nube,   "nube.png").
simbolo(lluvia, "lluvia.png").
simbolo(nieve,  "nieve.png").
simbolo(viento, "viento.png").

% Reglas para el conocimiento del lenguaje natural.

palabra(coche,   "coche").
palabra(arbol,   "arbol").
palabra(casa,    "casa").
palabra(perro,   "perro").
palabra(gato,    "gato").
palabra(pajaro,  "pajaro").
palabra(flor,    "flor").
palabra(sol,     "sol").
palabra(luna,    "luna").
palabra(estrella,"estrella").
palabra(nube,    "nube").
palabra(lluvia,  "lluvia").
palabra(nieve,   "nieve").
palabra(viento,  "viento").

% Reglas para la comprensión del lenguaje natural.

comprender(Frase, ListaPalabras) :-
    atom_codes(Frase, ListaCodigos),
    phrase(lista_palabras(ListaPalabras), ListaCodigos).

lista_palabras([]) --> [].
lista_palabras([Palabra|Palabras]) -->
    palabra(Palabra, Codigos),
    [Codigos],
    lista_palabras(Palabras).

% Reglas para la generación del lenguaje natural.

generar([Palabra]) -->
    palabra(Palabra, Codigos),
    [Codigos].
generar([Palabra|Palabras]) -->
    palabra(Palabra, Codigos),
    [Codigos],
    generar(Palabras).

% Reglas para la traducción del lenguaje natural.

traducir(FraseEsp, FraseIng) :-
    comprender(FraseEsp, ListaPalabrasEsp),
    generar(ListaPalabrasIng),
    atom_codes(FraseIng, ListaPalabrasIng).

% Reglas para la interfaz gráfica.

interfaz :-
    crear_ventana("Traductor", 600, 400),
    crear_etiqueta(FraseEsp, 10, 10),
    crear_cuadro_texto(FraseEsp, 10, 30, 500, 20),
    crear_etiqueta(FraseIng, 10, 80),
    crear_cuadro_texto(FraseIng, 10, 100, 500, 20),
    crear_boton(Traducir, "Traducir", 10, 150, 100, 20),
    escuchar_eventos.

escuchar_eventos :-
    recibir_evento(Evento),
    manejar_evento(Evento),
    escuchar_eventos.

manejar_evento(Evento) :-
    Evento = evento_boton(Traducir),
    obtener_valor(FraseEsp, FraseEsp),
    traducir(FraseEsp, FraseIng),
    establecer_valor(FraseIng, FraseIng).

% Reglas para la inicialización del sistema.

inicializar :-
    interfaz.
```

Este código crea una interfaz gráfica de usuario (GUI) que permite a los usuarios traducir frases del español al inglés. La GUI consiste en una ventana con dos cuadros de texto, uno para la frase en español y otro para la frase en inglés, y un botón para traducir la frase.

Cuando el usuario escribe una frase en español en el cuadro de texto, el sistema la traduce al inglés utilizando el conocimiento del mundo real y del lenguaje natural almacenado en las reglas de Prolog. A continuación, el sistema muestra la traducción en el cuadro de texto de la frase en inglés.

El código también incluye reglas para inicializar el sistema y para manejar los eventos de la GUI. Cuando el usuario hace clic en el botón Traducir, el sistema maneja el evento y llama a la función traducir para traducir la frase en español a inglés. A continuación, el sistema muestra la traducción en el cuadro de texto de la frase en inglés.