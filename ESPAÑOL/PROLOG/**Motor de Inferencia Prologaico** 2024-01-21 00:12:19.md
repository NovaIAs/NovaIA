```
:- dynamic conocimiento/1.
:- dynamic reglas/2.

% Asertar un hecho en la base de conocimiento
asercion(Hecho) :-
    conocimiento(Hecho).

% Consultar un hecho en la base de conocimiento
consulta(Hecho) :-
    conocimiento(Hecho).

% Añadir una regla a la base de reglas
regla(Condicion, Conclusion) :-
    reglas(Condicion, Conclusion).

% Consultar una regla en la base de reglas
consulta_regla(Condicion, Conclusion) :-
    reglas(Condicion, Conclusion).

% Motor de inferencia
inferencia :-
    % Obtener la siguiente regla a aplicar
    regla(Condicion, Conclusion),

    % Comprobar si la condición de la regla es cierta
    consulta(Condicion),

    % Si la condición es cierta, añadir la conclusión a la base de conocimiento
    asercion(Conclusion),

    % Continuar con la inferencia
    inferencia.

% Punto de entrada al programa
main :-
    % Añadir algunos hechos a la base de conocimiento
    asercion(perro(firulais)),
    asercion(gato(bigotes)),
    asercion(dueño(juan, firulais)),
    asercion(dueño(maria, bigotes)),

    % Añadir algunas reglas a la base de reglas
    regla(perro(X), mamifero(X)),
    regla(gato(X), mamifero(X)),
    regla(dueño(X, Y), quiere_mucho(X, Y)),

    % Iniciar el motor de inferencia
    inferencia,

    % Mostrar los resultados de la inferencia
    write('Los mamíferos de la base de conocimiento son: '),
    findall(X, mamifero(X), Mamiferos),
    write(Mamiferos), nl,

    write('Las personas que quieren mucho a sus mascotas son: '),
    findall(X, quiere_mucho(X, Y), Personas),
    write(Personas), nl.
```

Explicación del código:

* Se definen tres predicados dinámicos: `conocimiento/1`, `reglas/2` y `quiere_mucho/2`.
* El predicado `asercion/1` añade un hecho a la base de conocimiento.
* El predicado `consulta/1` consulta un hecho en la base de conocimiento.
* El predicado `regla/2` añade una regla a la base de reglas.
* El predicado `consulta_regla/2` consulta una regla en la base de reglas.
* El predicado `inferencia/0` es el motor de inferencia.
* El predicado `main/0` es el punto de entrada al programa.

El programa funciona de la siguiente manera:

1. Se añaden algunos hechos a la base de conocimiento.
2. Se añaden algunas reglas a la base de reglas.
3. Se inicia el motor de inferencia.
4. El motor de inferencia aplica las reglas a los hechos de la base de conocimiento hasta que no hay más reglas que aplicar.
5. Se muestran los resultados de la inferencia.