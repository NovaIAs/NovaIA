```f#

// Definición del tipo de datos abstracto de una lista enlazada.
type Lista =
    | Nil // La lista vacía.
    | Cons(cabeza: int, cola: Lista) // Una lista con una cabeza y una cola.

// Función que comprueba si una lista está vacía.
let estaVacia (lista: Lista) =
    match lista with
    | Nil -> true
    | Cons(_, _) -> false

// Función que devuelve la cabeza de una lista.
let cabeza (lista: Lista) =
    match lista with
    | Nil -> failwith "La lista está vacía."
    | Cons(cabeza, _) -> cabeza

// Función que devuelve la cola de una lista.
let cola (lista: Lista) =
    match lista with
    | Nil -> failwith "La lista está vacía."
    | Cons(_, cola) -> cola

// Función que añade un elemento a la cabeza de una lista.
let añadirCabeza (elemento: int, lista: Lista) =
    Cons(elemento, lista)

// Función que añade un elemento a la cola de una lista.
let añadirCola (lista: Lista, elemento: int) =
    match lista with
    | Nil -> Cons(elemento, Nil)
    | Cons(cabeza, cola) -> Cons(cabeza, añadirCola(cola, elemento))

// Función que elimina el primer elemento de una lista.
let eliminarCabeza (lista: Lista) =
    match lista with
    | Nil -> failwith "La lista está vacía."
    | Cons(_, cola) -> cola

// Función que elimina el último elemento de una lista.
let eliminarCola (lista: Lista) =
    match lista with
    | Nil -> failwith "La lista está vacía."
    | Cons(cabeza, Nil) -> Nil
    | Cons(cabeza, cola) -> Cons(cabeza, eliminarCola(cola))

// Función que devuelve el número de elementos de una lista.
let longitud (lista: Lista) =
    let rec longitudAux (lista: Lista, contador: int) =
        match lista with
        | Nil -> contador
        | Cons(_, cola) -> longitudAux(cola, contador + 1)
    longitudAux(lista, 0)

// Función que devuelve el máximo elemento de una lista.
let máximo (lista: Lista) =
    let rec máximoAux (lista: Lista, máximoActual: int) =
        match lista with
        | Nil -> máximoActual
        | Cons(elemento, cola) ->
            let nuevoMáximo = if elemento > máximoActual then elemento else máximoActual
            máximoAux(cola, nuevoMáximo)
    máximoAux(lista, Int32.MinValue)

// Función que devuelve el mínimo elemento de una lista.
let mínimo (lista: Lista) =
    let rec mínimoAux (lista: Lista, mínimoActual: int) =
        match lista with
        | Nil -> mínimoActual
        | Cons(elemento, cola) ->
            let nuevoMínimo = if elemento < mínimoActual then elemento else mínimoActual
            mínimoAux(cola, nuevoMínimo)
    mínimoAux(lista, Int32.MaxValue)

// Función que devuelve la suma de los elementos de una lista.
let suma (lista: Lista) =
    let rec sumaAux (lista: Lista, sumaActual: int) =
        match lista with
        | Nil -> sumaActual
        | Cons(elemento, cola) -> sumaAux(cola, sumaActual + elemento)
    sumaAux(lista, 0)

// Función que devuelve el producto de los elementos de una lista.
let producto (lista: Lista) =
    let rec productoAux (lista: Lista, productoActual: int) =
        match lista with
        | Nil -> productoActual
        | Cons(elemento, cola) -> productoAux(cola, productoActual * elemento)
    productoAux(lista, 1)

// Función que devuelve el elemento que se encuentra en una posición determinada de una lista.
let elementoEnPosición (posición: int, lista: Lista) =
    let rec elementoEnPosiciónAux (posiciónActual: int, lista: Lista) =
        match lista with
        | Nil -> failwith "La posición no existe."
        | Cons(elemento, cola) ->
            if posiciónActual = posición then elemento
            else elementoEnPosiciónAux(posiciónActual + 1, cola)
    elementoEnPosiciónAux(0, lista)

// Función que devuelve una lista con los elementos de una lista invertidos.
let invertirLista (lista: Lista) =
    let rec invertirListaAux (lista: Lista, listaInvertida: Lista) =
        match lista with
        | Nil -> listaInvertida
        | Cons(elemento, cola) -> invertirListaAux(cola, añadirCabeza(elemento, listaInvertida))
    invertirListaAux(lista, Nil)

// Función que ordena una lista de forma ascendente.
let ordenarLista (lista: Lista) =
    let rec ordenarListaAux (lista: Lista, listaOrdenada: Lista) =
        match lista with
        | Nil -> listaOrdenada
        | Cons(elemento, cola) ->
            let nuevoElemento = mínimo(lista)
            let nuevaLista = eliminarCola(lista, nuevoElemento)
            ordenarListaAux(nuevaLista, añadirCabeza(nuevoElemento, listaOrdenada))
    ordenarListaAux(lista, Nil)

// Función que devuelve una lista con los elementos de una lista filtrados por una condición.
let filtrarLista (condición: int -> bool, lista: Lista) =
    let rec filtrarListaAux (condición: int -> bool, lista: Lista, listaFiltrada: Lista) =
        match lista with
        | Nil -> listaFiltrada
        | Cons(elemento, cola) ->
            if condición(elemento) then
                filtrarListaAux(condición, cola, añadirCabeza(elemento, listaFiltrada))
            else
                filtrarListaAux(condición, cola, listaFiltrada)
    filtrarListaAux(condición, lista, Nil)

```

Explicación del código:

* Este código define una lista enlazada como un tipo de datos abstracto.
* La lista enlazada puede estar vacía o puede tener una cabeza y una cola.
* El código define varias funciones que operan sobre listas enlazadas, incluyendo funciones para añadir y eliminar elementos, devolver la cabeza o la cola de una lista, comprobar si una lista está vacía, devolver el número de elementos de una lista, devolver el máximo o el mínimo elemento de una lista, devolver la suma o el producto de los elementos de una lista, devolver el elemento en una posición determinada de una lista, invertir una lista, ordenar una lista y filtrar una lista por una condición.
* Estas funciones son recursivas y utilizan el patrón de coincidencia para procesar los diferentes casos posibles.