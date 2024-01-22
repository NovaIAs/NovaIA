```prolog
:- dynamic ingrediente/3, plato/3, receta/3.

% Ingredientes

ingrediente(azucar, dulce, 100).
ingrediente(harina, neutro, 200).
ingrediente(mantequilla, graso, 150).
ingrediente(huevo, proteina, 2).
ingrediente(leche, neutro, 200).
ingrediente(chocolate, dulce, 50).
ingrediente(vainilla, dulce, 5).

% Platos

plato(tarta, dulce, 6).
plato(bizcocho, dulce, 8).
plato(galletas, dulce, 12).
plato(pan, neutro, 4).
plato(pizza, neutro, 4).
plato(ensalada, neutro, 2).

% Recetas

receta(tarta, [
    ingrediente(azucar, 100),
    ingrediente(harina, 200),
    ingrediente(mantequilla, 150),
    ingrediente(huevo, 2),
    ingrediente(leche, 200),
    ingrediente(chocolate, 50),
    ingrediente(vainilla, 5)
], 60).

receta(bizcocho, [
    ingrediente(azucar, 150),
    ingrediente(harina, 250),
    ingrediente(mantequilla, 100),
    ingrediente(huevo, 3),
    ingrediente(leche, 150),
    ingrediente(vainilla, 10)
], 45).

receta(galletas, [
    ingrediente(azucar, 100),
    ingrediente(harina, 200),
    ingrediente(mantequilla, 150),
    ingrediente(huevo, 1),
    ingrediente(vainilla, 5)
], 30).

receta(pan, [
    ingrediente(harina, 500),
    ingrediente(agua, 300),
    ingrediente(levadura, 10),
    ingrediente(sal, 10)
], 120).

receta(pizza, [
    ingrediente(harina, 250),
    ingrediente(agua, 150),
    ingrediente(levadura, 5),
    ingrediente(sal, 5),
    ingrediente(tomate, 200),
    ingrediente(queso, 150),
    ingrediente(jamon, 100)
], 60).

receta(ensalada, [
    ingrediente(lechuga, 100),
    ingrediente(tomate, 100),
    ingrediente(pepino, 100),
    ingrediente(cebolla, 50),
    ingrediente(aceite, 50),
    ingrediente(vinagre, 25),
    ingrediente(sal, 10),
    ingrediente(pimienta, 5)
], 15).

% Consultas

% ¿Qué ingredientes se necesitan para hacer una tarta?
ingredientes_tarta(Ingredientes) :-
    receta(tarta, Ingredientes, _).

% ¿Cuánto tiempo se tarda en hacer una pizza?
tiempo_pizza(Tiempo) :-
    receta(pizza, _, Tiempo).

% ¿Qué platos se pueden hacer con harina?
platos_con_harina(Platos) :-
    plato(Plato, _, _),
    receta(Plato, Ingredientes, _),
    member(ingrediente(harina, _, _), Ingredientes).

% ¿Cuáles son los ingredientes dulces?
ingredientes_dulces(Ingredientes) :-
    ingrediente(Ingrediente, dulce, _),
    Ingredientes = [Ingrediente | _].

% ¿Qué platos tienen más de 4 porciones?
platos_grandes(Platos) :-
    plato(Plato, _, Porciones),
    Porciones > 4,
    Platos = [Plato | _].

% ¿Cuáles son las recetas que tardan menos de 30 minutos?
recetas_rapidas(Recetas) :-
    receta(Receta, _, Tiempo),
    Tiempo < 30,
    Recetas = [Receta | _].
```

Explicación del código:

* El código anterior es un programa Prolog que incluye una base de datos de ingredientes, platos y recetas.
* Cada ingrediente se define con un nombre, un tipo (dulce, neutro o graso) y una cantidad.
* Cada plato se define con un nombre, un tipo (dulce o neutro) y un número de porciones.
* Cada receta se define con un nombre, una lista de ingredientes y un tiempo de preparación.
* El código incluye varias consultas que permiten obtener información de la base de datos. Por ejemplo, la consulta ``ingredientes_tarta(Ingredientes)`` devuelve la lista de ingredientes necesarios para hacer una tarta.
* El código también incluye una serie de reglas que permiten realizar inferencias a partir de la base de datos. Por ejemplo, la regla ``platos_grandes(Platos)`` devuelve una lista de los platos que tienen más de 4 porciones.
* El código se puede utilizar para crear un sistema de gestión de recetas que permita a los usuarios buscar recetas, obtener información sobre los ingredientes y los platos, y crear sus propias recetas.