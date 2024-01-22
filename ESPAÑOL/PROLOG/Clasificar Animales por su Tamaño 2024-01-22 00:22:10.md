```prolog
% Regla 1: Si el animal es un perro y el perro es pequeño, entonces el animal es un perro pequeño.
perro_pequeno(X) :- perro(X), pequeno(X).

% Regla 2: Si el animal es un gato y el gato es pequeño, entonces el animal es un gato pequeño.
gato_pequeno(X) :- gato(X), pequeno(X).

% Regla 3: Si el animal es un pez y el pez es pequeño, entonces el animal es un pez pequeño.
pez_pequeno(X) :- pez(X), pequeno(X).

% Regla 4: Si el animal es un ave y el ave es pequeña, entonces el animal es un ave pequeña.
ave_pequena(X) :- ave(X), pequena(X).

% Regla 5: Si el animal es un mamífero y el mamífero es pequeño, entonces el animal es un mamífero pequeño.
mamifero_pequeno(X) :- mamifero(X), pequeno(X).

% Regla 6: Si el animal es un reptil y el reptil es pequeño, entonces el animal es un reptil pequeño.
reptil_pequeno(X) :- reptil(X), pequeno(X).

% Regla 7: Si el animal es un anfibio y el anfibio es pequeño, entonces el animal es un anfibio pequeño.
anfibio_pequeno(X) :- anfibio(X), pequeno(X).

% Regla 8: Si el animal es un invertebrado y el invertebrado es pequeño, entonces el animal es un invertebrado pequeño.
invertebrado_pequeno(X) :- invertebrado(X), pequeno(X).

% Regla 9: Si el animal es un perro y el perro es grande, entonces el animal es un perro grande.
perro_grande(X) :- perro(X), grande(X).

% Regla 10: Si el animal es un gato y el gato es grande, entonces el animal es un gato grande.
gato_grande(X) :- gato(X), grande(X).

% Regla 11: Si el animal es un pez y el pez es grande, entonces el animal es un pez grande.
pez_grande(X) :- pez(X), grande(X).

% Regla 12: Si el animal es un ave y el ave es grande, entonces el animal es un ave grande.
ave_grande(X) :- ave(X), grande(X).

% Regla 13: Si el animal es un mamífero y el mamífero es grande, entonces el animal es un mamífero grande.
mamifero_grande(X) :- mamifero(X), grande(X).

% Regla 14: Si el animal es un reptil y el reptil es grande, entonces el animal es un reptil grande.
reptil_grande(X) :- reptil(X), grande(X).

% Regla 15: Si el animal es un anfibio y el anfibio es grande, entonces el animal es un anfibio grande.
anfibio_grande(X) :- anfibio(X), grande(X).

% Regla 16: Si el animal es un invertebrado y el invertebrado es grande, entonces el animal es un invertebrado grande.
invertebrado_grande(X) :- invertebrado(X), grande(X).

% Hechos:

% El perro llamado "Fido" es pequeño.
perro_pequeno(fido).

% El gato llamado "Mishi" es grande.
gato_grande(mishi).

% El pez llamado "Nemo" es pequeño.
pez_pequeno(nemo).

% El ave llamada "Piolín" es pequeña.
ave_pequena(piolín).

% El mamífero llamado "Elefante" es grande.
mamifero_grande(elefante).

% El reptil llamado "Cocodrilo" es grande.
reptil_grande(cocodrilo).

% El anfibio llamado "Rana" es pequeña.
anfibio_pequeno(rana).

% El invertebrado llamado "Hormiga" es pequeño.
invertebrado_pequeno(hormiga).

```

Este código define una serie de reglas y hechos en PROLOG que describen el tamaño y el tipo de diferentes animales. Las reglas utilizan una sintaxis similar a la del español, con verbos y sustantivos. Los hechos son declaraciones sobre el mundo, como el hecho de que el perro llamado "Fido" es pequeño.

Para utilizar este código, primero debes cargarlo en un intérprete PROLOG. Una vez cargado, puedes utilizar las reglas para hacer preguntas sobre el tamaño y el tipo de diferentes animales. Por ejemplo, puedes escribir la siguiente consulta:

```prolog
?- perro_pequeno(fido).
```

Esto devolverá el resultado "sí", porque el hecho "perro_pequeno(fido)" es cierto.

También puedes utilizar las reglas para hacer preguntas más complejas. Por ejemplo, puedes escribir la siguiente consulta:

```prolog
?- X :- perro(X), pequeno(X).
```

Esto devolverá una lista de todos los perros que son pequeños.

Este código es sólo un ejemplo de cómo se puede utilizar PROLOG para representar y razonar sobre el conocimiento. PROLOG es un lenguaje muy potente que puede utilizarse para resolver una amplia variedad de problemas.