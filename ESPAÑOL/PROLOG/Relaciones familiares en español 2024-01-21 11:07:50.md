```prolog
:- dynamic padre/2, madre/2, hijo/2, hija/2, hermano/2, hermana/2, abuelo/2, abuela/2, nieto/2, nieta/2, primo/2, prima/2, tio/2, tia/2, sobrino/2, sobrina/2, cuñado/2, cuñada/2.

% Reglas para agregar relaciones familiares.

padre(Padre, Hijo) :- assert(padre(Padre, Hijo)).
madre(Madre, Hija) :- assert(madre(Madre, Hija)).
hijo(Hijo, Padre) :- padre(Padre, Hijo).
hija(Hija, Madre) :- madre(Madre, Hija).
hermano(Hermano, Hermana) :-
    padre(Padre, Hermano),
    padre(Padre, Hermana),
    Hermano \= Hermana.
hermana(Hermana, Hermano) :- hermano(Hermano, Hermana).
abuelo(Abuelo, Nieto) :-
    padre(Abuelo, Padre),
    padre(Padre, Nieto).
abuela(Abuela, Nieta) :-
    madre(Abuela, Madre),
    madre(Madre, Nieta).
nieto(Nieto, Abuelo) :- abuelo(Abuelo, Nieto).
nieta(Nieta, Abuela) :- abuela(Abuela, Nieta).
primo(Primo, Prima) :-
    padre(Padre1, Primo),
    hermano(Padre1, Padre2),
    padre(Padre2, Prima).
prima(Prima, Primo) :- primo(Primo, Prima).
tio(Tio, Sobrino) :-
    hermano(Tio, Padre),
    padre(Padre, Sobrino).
tia(Tia, Sobrina) :-
    hermana(Tia, Madre),
    madre(Madre, Sobrina).
sobrino(Sobrino, Tio) :- tio(Tio, Sobrino).
sobrina(Sobrina, Tia) :- tia(Tia, Sobrina).
cuñado(Cuñado, Cuñada) :-
    hermano(Cuñado, Marido),
    padre(Marido, Hijo),
    madre(Cuñada, Hijo).
cuñada(Cuñada, Cuñado) :- cuñado(Cuñado, Cuñada).

% Reglas para consultar relaciones familiares.

padre_de(Padre, Hijo) :- padre(Padre, Hijo).
madre_de(Madre, Hija) :- madre(Madre, Hija).
hijo_de(Hijo, Padre) :- padre(Padre, Hijo).
hija_de(Hija, Madre) :- madre(Madre, Hija).
hermano_de(Hermano, Hermana) :- hermano(Hermano, Hermana).
hermana_de(Hermana, Hermano) :- hermana(Hermana, Hermano).
abuelo_de(Abuelo, Nieto) :- abuelo(Abuelo, Nieto).
abuela_de(Abuela, Nieta) :- abuela(Abuela, Nieta).
nieto_de(Nieto, Abuelo) :- nieto(Nieto, Abuelo).
nieta_de(Nieta, Abuela) :- nieta(Nieta, Abuela).
primo_de(Primo, Prima) :- primo(Primo, Prima).
prima_de(Prima, Primo) :- prima(Prima, Primo).
tio_de(Tio, Sobrino) :- tio(Tio, Sobrino).
tia_de(Tia, Sobrina) :- tia(Tia, Sobrina).
sobrino_de(Sobrino, Tio) :- sobrino(Sobrino, Tio).
sobrina_de(Sobrina, Tia) :- sobrina(Sobrina, Tia).
cuñado_de(Cuñado, Cuñada) :- cuñado(Cuñado, Cuñada).
cuñada_de(Cuñada, Cuñado) :- cuñada(Cuñada, Cuñado).

% Ejemplo de uso.

padre('Juan', 'Pedro').
madre('María', 'Pedro').
padre('Juan', 'Ana').
madre('María', 'Ana').
hermano('Pedro', 'Ana').
hermana('Ana', 'Pedro').
abuelo('Luis', 'Pedro').
abuelo('Luis', 'Ana').
abuela('Rosa', 'Pedro').
abuela('Rosa', 'Ana').
nieto('Pedro', 'Luis').
nieta('Pedro', 'Rosa').
nieto('Ana', 'Luis').
nieta('Ana', 'Rosa').
primo('Pedro', 'Carlos').
prima('Ana', 'Carolina').
tio('Luis', 'Carlos').
tia('Rosa', 'Carolina').
sobrino('Carlos', 'Luis').
sobrina('Carolina', 'Rosa').
cuñado('Pedro', 'Carlos').
cuñada('Ana', 'Carolina').

% Consulta.

padre_de('Juan', 'Pedro').
madre_de('María', 'Ana').
hermano_de('Pedro', 'Ana').
abuelo_de('Luis', 'Pedro').
abuela_de('Rosa', 'Ana').
nieto_de('Pedro', 'Luis').
nieta_de('Ana', 'Rosa').
primo_de('Pedro', 'Carlos').
prima_de('Ana', 'Carolina').
tio_de('Luis', 'Carlos').
tia_de('Rosa', 'Carolina').
sobrino_de('Carlos', 'Luis').
sobrina_de('Carolina', 'Rosa').
cuñado_de('Pedro', 'Carlos').
cuñada_de('Ana', 'Carolina').
```

Explicación:

- El código utiliza hechos para definir relaciones familiares, como padre/2, madre/2, hijo/2, hija/2, hermano/2, hermana/2, abuelo/2, abuela/2, nieto/2, nieta/2, primo/2, prima/2, tio/2, tia/2, sobrino/2, sobrina/2, cuñado/2 y cuñada/2.


- El código también define reglas para inferir nuevas relaciones familiares basadas en las relaciones existentes. Por ejemplo, la regla hermano/2 infiere que dos personas son hermanos si tienen el mismo padre.


- El código proporciona una interfaz para agregar y consultar relaciones familiares.


- El ejemplo de uso muestra cómo utilizar el código para definir y consultar relaciones familiares.


- El código está escrito en español, lo que lo hace más fácil de entender para los hablantes de español.


Este código es complejo porque define una amplia gama de relaciones familiares y proporciona una interfaz para agregar y consultar esas relaciones. También está escrito en español, lo que lo hace más fácil de entender para los hablantes de español.