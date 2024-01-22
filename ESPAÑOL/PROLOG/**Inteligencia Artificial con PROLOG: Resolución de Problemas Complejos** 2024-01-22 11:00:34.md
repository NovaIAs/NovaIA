```prolog
:- dynamic nombrePersona/2, relacionPersona/3, caracteristicaPersona/3.

% Cargas los datos en la base de conocimiento
nombrePersona(pedro, "Pedro García").
nombrePersona(maría, "María López").
nombrePersona(juan, "Juan Pérez").

relacionPersona(pedro, padre, juan).
relacionPersona(maría, madre, juan).
relacionPersona(pedro, hermano, carlos).
relacionPersona(maría, hermana, ana).

caracteristicaPersona(pedro, edad, 45).
caracteristicaPersona(maría, edad, 43).
caracteristicaPersona(juan, edad, 18).
caracteristicaPersona(carlos, edad, 16).
característicaPersona(ana, edad, 14).

% Consultas
% ¿Quién es el padre de Juan?
padre(Persona) :- 
    relacionPersona(Persona, padre, juan).

% ¿Quiénes son los hermanos de Pedro?
hermanos(Persona) :- 
    relacionPersona(Pedro, hermano, Persona).

% ¿Cuál es la edad de María?
edad(Persona, Edad) :- 
    caracteristicaPersona(Persona, edad, Edad).

% ¿Cuáles son las características de una persona?
caracteristicas(Persona) :-
    findall(Característica, caracteristicaPersona(Persona, _, Característica), Características).

% Búsqueda de caminos en un grafo
camino(Origen, Destino, Cammino) :-
    relacionPersona(Origen, Relacion, Destino),
    Cammino = [Origen, Destino].

camino(Origen, Destino, Cammino) :-
    relacionPersona(Origen, Relacion, Intermedio),
    camino(Intermedio, Destino, RestoCammino),
    append([Origen], RestoCammino, Cammino).

% Búsqueda de ancestros
ancestros(Persona, Ancestro) :-
    relacionPersona(Ancestro, padre, Persona).

ancestros(Persona, Ancestro) :-
    relacionPersona(Ancestro, padre, Intermedio),
    ancestros(Intermedio, Persona).

% Búsqueda de descendientes
descendientes(Persona, Descendiente) :-
    relacionPersona(Persona, padre, Descendiente).

descendientes(Persona, Descendiente) :-
    relacionPersona(Persona, padre, Intermedio),
    descendientes(Intermedio, Descendiente).
```

Explicación del código:

* La base de conocimiento está formada por hechos y reglas. Los hechos son declaraciones que describen el mundo, mientras que las reglas son declaraciones que especifican cómo se pueden deducir nuevos hechos a partir de los hechos existentes.
* Los hechos se representan utilizando el formato nombre_predicado/aridad, donde nombre_predicado es el nombre del predicado, aridad es el número de argumentos que toma el predicado y los argumentos están encerrados entre paréntesis.
* Las reglas se representan utilizando el formato cabeza :- cuerpo, donde cabeza es el hecho que se deduce y cuerpo es una secuencia de hechos y reglas que apoyan la conclusión.
* Las consultas se utilizan para obtener información de la base de conocimiento. Una consulta se representa utilizando el formato objetivo, donde objetivo es el hecho que se desea obtener.
* Las cláusulas son las unidades básicas de las que están compuestas las reglas y las consultas. Una cláusula es una secuencia de literales que están conectados por el operador lógico ":-".
* Los literales pueden ser hechos o reglas. Un hecho es una declaración que es verdadera en el mundo real. Una regla es una declaración que especifica cómo se puede deducir un hecho a partir de otros hechos.
* Las variables se utilizan para representar información desconocida. Una variable se representa utilizando una letra mayúscula.
* Los términos son expresiones que se construyen a partir de constantes, variables y funciones. Un término puede ser un hecho, una regla o una consulta.