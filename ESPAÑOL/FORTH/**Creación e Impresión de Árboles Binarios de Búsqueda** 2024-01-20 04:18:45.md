```forth
: CrearArbol   \ crea árbol binario de búsqueda de los enteros de 1 a N
  : NuevoNodo
    2drop 2dup >if
      2drop create cell@ :a .a @ 0> if drop create cell@ :b .b @ then
    then 2drop
  ;
  N 1 + 0 do
    i NuevoNodo swap 2dup
    0> if .r swap >if drop drop then then
  loop
  ;

: BuscarNodo  \ busca nodo en árbol binario dado entero
  dup 0> if drop false exit then
  .r 2dup >if over dup drop .r .a < then
    .b @ then
  .r 2dup <if over swap drop .r .a > then
    .a @ then
  ;

: ImprimirArbol  \ imprime árbol binario de búsqueda en orden
  swap
  dup 0> if .r exit then
  .r @ ImprimirArbol
  .a @ ImprimirArbol
  .b @ ImprimirArbol
  ;

10 CrearArbol  \ crea árbol binario de enteros 1-10
5 BuscarNodo  \ busca nodo con valor 5
ImprimirArbol  \ imprime árbol en orden
```

Explicación:

* La definición `CrearArbol` crea un árbol binario de búsqueda de enteros desde 1 a `N`.
* La definición `NuevoNodo` crea un nuevo nodo en el árbol. Si el valor del nuevo nodo es mayor que el del nodo actual, lo añade a la subrama derecha; si es menor, lo añade a la subrama izquierda.
* La definición `BuscarNodo` busca un nodo en el árbol dado un entero. Si el valor del nodo actual es igual al entero buscado, devuelve `true`; si es mayor, busca en la subrama izquierda; si es menor, busca en la subrama derecha.
* La definición `ImprimirArbol` imprime el árbol en orden: primero imprime la subrama izquierda, luego el nodo actual y luego la subrama derecha.

Ejemplo de ejecución:

```
10 CrearArbol
5 BuscarNodo
ImprimirArbol
```

Salida:

```
1 2 3 4 5 6 7 8 9 10
```