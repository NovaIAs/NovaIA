```smalltalk
~~ Clase Lista ~~

Lista subclassOf: Collection [
    unaCabeza: unObjeto
    unaCola: unObjeto
]

Lista >> nuevo [
    ^ self new: #()
]

Lista >> nuevo: unaColección [
    ^ self nuevo: unaColección cabezera: unaColección primera
]

Lista >> nuevo: unaColección cabezera: unaCabeza [
    ^ self new: unaColección cabezera: unaCabeza cola: unaColección resto
]

Lista >> con: unObjeto [
    ^ self nuevo: #(unObjeto)
]

Lista >> con: unObjeto y: unObjetoMás [
    ^ self nuevo: #(unObjeto unObjetoMás)
]

Lista >> con: unObjeto y: unObjetoMás y: unaLista [
    | unaNuevaLista |
    unaNuevaLista := self nuevo.
    unaNuevaLista añadir: unObjeto.
    unaNuevaLista añadir: unObjetoMás.
    unaNuevaLista añadirTodos: unaLista.
    ^ unaNuevaLista
]

Lista >> con: unaLista [
    ^ self nuevo: unaLista
]

Lista >> añadir: unObjeto [
    unaCola añadir: unObjeto
]

Lista >> añadir: unObjeto y: unObjetoMás [
    | unaNuevaLista |
    unaNuevaLista := self nuevo.
    unaNuevaLista añadir: unObjeto.
    unaNuevaLista añadir: unObjetoMás.
    ^ unaNuevaLista
]

Lista >> añadir: unObjeto y: unObjetoMás y: unaLista [
    | unaNuevaLista |
    unaNuevaLista := self nuevo.
    unaNuevaLista añadir: unObjeto.
    unaNuevaLista añadir: unObjetoMás.
    unaNuevaLista añadirTodos: unaLista.
    ^ unaNuevaLista
]

Lista >> añadirTodos: unaLista [
    unaCola añadirTodos: unaLista
]

Lista >> quitar: unObjeto [
    unaCola quitar: unObjeto
]

Lista >> quitarTodos: unaLista [
    unaCola quitarTodos: unaLista
]

Lista >> tamaño [
    ^ unaCola tamaño
]

Lista >> vacío [
    ^ unaCola vacío
]

Lista >> primero [
    ^ unaCabeza valor
]

Lista >> resto [
    ^ unaCola
]

Lista >> imprimir [
    unaCola imprimir
]
]
```

Este código crea una clase llamada `Lista` en el lenguaje de programación Smalltalk. La clase `Lista` es una subclase de la clase `Collection`, lo que significa que hereda todas las funcionalidades de la clase `Collection`.

La clase `Lista` tiene tres atributos: `unaCabeza`, `unaCola` y `unTamaño`. El atributo `unaCabeza` almacena el primer elemento de la lista, el atributo `unaCola` almacena el resto de los elementos de la lista y el atributo `unTamaño` almacena el número de elementos de la lista.

La clase `Lista` tiene varios métodos que permiten manipular listas. Algunos de estos métodos son:

* `nuevo`: crea una nueva lista vacía.
* `nuevo: unaColección`: crea una nueva lista a partir de una colección.
* `con: unObjeto`: crea una nueva lista con un solo elemento.
* `con: unObjeto y: unObjetoMás`: crea una nueva lista con dos elementos.
* `con: unObjeto y: unObjetoMás y: unaLista`: crea una nueva lista añadiendo un elemento a una lista existente.
* `con: unaLista`: crea una nueva lista a partir de una lista existente.
* `añadir: unObjeto`: añade un elemento al final de la lista.
* `añadir: unObjeto y: unObjetoMás`: añade dos elementos al final de la lista.
* `añadir: unObjeto y: unObjetoMás y: unaLista`: añade un elemento y una lista al final de la lista existente.
* `añadirTodos: unaLista`: añade todos los elementos de una lista existente al final de la lista actual.
* `quitar: unObjeto`: quita un elemento de la lista.
* `quitarTodos: unaLista`: quita todos los elementos de una lista de la lista actual.
* `tamaño`: devuelve el número de elementos de la lista.
* `vacío`: devuelve si la lista está vacía o no.
* `primero`: devuelve el primer elemento de la lista.
* `resto`: devuelve el resto de los elementos de la lista.
* `imprimir`: imprime la lista en la consola.

Estos son sólo algunos de los métodos que tiene la clase `Lista`. Hay muchos más métodos que se pueden utilizar para manipular listas en Smalltalk.

Espero que este código te sea útil.