```erlang
% Define a función para contar el número de ocurrencias de un elemento en una lista.
-module(contar_ocurrencias).
-export([contar/2]).

contar(Elemento, Lista) ->
  % Usa la función recursiva interna para contar las ocurrencias.
  contar_interno(Elemento, Lista, 0).

% Función recursiva interna para contar las ocurrencias.
contar_interno(_, [], Conteo) ->
  % Si la lista está vacía, devuelve el conteo.
  Conteo;
contar_interno(Elemento, [Cabeza | Resto], Conteo) ->
  % Si el elemento de la cabeza coincide con el elemento a contar, incrementa el conteo.
  if
    Cabeza =:= Elemento ->
      contar_interno(Elemento, Resto, Conteo + 1);
    true ->
      % Si el elemento de la cabeza no coincide, sigue contando en el resto de la lista.
      contar_interno(Elemento, Resto, Conteo)
  end.

% Define una función para ordenar una lista de números.
-module(ordenar).
-export([ordenar/1]).

ordenar(Lista) ->
  % Usa la función interna para ordenar la lista.
  ordenar_interno(Lista, []).

% Función interna para ordenar la lista.
ordenar_interno([], ListaOrdenada) ->
  % Si la lista está vacía, devuelve la lista ordenada.
  ListaOrdenada;
ordenar_interno([Cabeza | Resto], ListaOrdenada) ->
  % Inserta el elemento de la cabeza en la posición correcta de la lista ordenada.
  ordenar_interno(Resto, insertar(Cabeza, ListaOrdenada)).

% Función interna para insertar un elemento en una lista ordenada.
insertar(Elemento, []) ->
  % Si la lista está vacía, devuelve una nueva lista con el elemento.
  [Elemento];
insertar(Elemento, [Cabeza | Resto]) ->
  % Compara el elemento con el elemento de la cabeza.
  if
    Elemento =< Cabeza ->
      % Si el elemento es menor o igual que el elemento de la cabeza, 
      % insértalo antes del elemento de la cabeza.
      [Elemento | Cabeza | Resto];
    true ->
      % Si el elemento es mayor que el elemento de la cabeza, 
      % insértalo después del elemento de la cabeza.
      [Cabeza | insertar(Elemento, Resto)]
  end.

% Define una función para invertir una lista.
-module(invertir).
-export([invertir/1]).

invertir(Lista) ->
  % Usa la función interna para invertir la lista.
  invertir_interno(Lista, []).

% Función interna para invertir la lista.
invertir_interno([], ListaInvertida) ->
  % Si la lista está vacía, devuelve la lista invertida.
  ListaInvertida;
invertir_interno([Cabeza | Resto], ListaInvertida) ->
  % Añade el elemento de la cabeza al principio de la lista invertida.
  invertir_interno(Resto, [Cabeza | ListaInvertida]).

% Define una función para sumar los elementos de una lista de números.
-module(sumar).
-export([sumar/1]).

sumar(Lista) ->
  % Usa la función interna para sumar los elementos de la lista.
  sumar_interno(Lista, 0).

% Función interna para sumar los elementos de la lista.
sumar_interno([], Suma) ->
  % Si la lista está vacía, devuelve la suma.
  Suma;
sumar_interno([Cabeza | Resto], Suma) ->
  % Suma el elemento de la cabeza a la suma actual.
  sumar_interno(Resto, Suma + Cabeza).
```

Este código contiene cuatro funciones:

1. **contar_ocurrencias/2**: Esta función cuenta el número de ocurrencias de un elemento en una lista.
2. **ordenar/1**: Esta función ordena una lista de números en orden ascendente.
3. **invertir/1**: Esta función invierte una lista.
4. **sumar/1**: Esta función suma los elementos de una lista de números.

El código está escrito en un estilo funcional, usando expresiones recursivas para realizar las operaciones. También utiliza la coincidencia de patrones para comparar los elementos de la lista.

Este código se puede usar en una variedad de aplicaciones, como el procesamiento de datos y la manipulación de listas.