```prolog
% Definición de las reglas gramaticales
oracion(Sujeto, Verbo, Objeto) :-
    sintagma_nominal(Sujeto),
    sintagma_verbal(Verbo, Objeto).

sintagma_nominal(Sujeto) :-
    articulo(Articulo),
    sustantivo(Sujeto, Articulo).

sintagma_verbal(Verbo, Objeto) :-
    verbo(Verbo),
    sintagma_nominal(Objeto).

articulo(el).
articulo(la).
articulo(los).
articulo(las).

sustantivo(perro, el).
sustantivo(perro, los).
sustantivo(casa, la).
sustantivo(casa, las).

verbo(come).
verbo(corre).
verbo(duerme).

% Consulta de la gramática
?- oracion(Sujeto, Verbo, Objeto).
```

Explicación del código:

* La primera línea define la regla gramatical para una oración. Una oración se compone de un sujeto, un verbo y un objeto, en ese orden.
* Las siguientes tres líneas definen las reglas gramaticales para un sintagma nominal (SN) y un sintagma verbal (SV). Un SN se compone de un artículo y un sustantivo. Un SV se compone de un verbo y un SN.
* Las siguientes cuatro líneas definen los artículos, sustantivos y verbos que pueden utilizarse en las reglas gramaticales.
* La última línea realiza una consulta a la gramática, preguntando por todas las oraciones válidas.

Este código es un ejemplo de cómo se puede utilizar la lógica de primer orden para representar y razonar sobre el lenguaje natural.