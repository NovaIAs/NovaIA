```smalltalk
| listaUno |
listaUno := Bag withAll: { 1 2 3 4 5 }.
listaUno add: 6.
listaUno size.  "6"
listaUno delete: 3.
listaUno includes: 4. "true"
listaUno at: 1. "1"
listaUno at: 7. "nil"
listaUno occurrencesOf: 2.  "1"

| listaDos |
listaDos := OrderedCollection new.
listaDos addAll: { 1 2 3 4 5 }.
listaDos add: 6.
listaDos at: 4.  "4"
listaDos first.  "1"
listaDos last.  "6"
listaDos indexOf: 3.  "3"
listaDos indexOf: 7.  "-1"
listaDos select: [ :x | x > 3 ].  "(4 5 6)"
listaDos sort.  "(1 2 3 4 5 6)"

| listaTres |
listaTres := Set withAll: { 1 2 3 4 5 }.
listaTres add: 2.
listaTres size.  "5"
listaTres delete: 3.
listaTres includes: 4.  "true"
listaTres union: { 6 7 }.  "{1 2 4 5 6 7}"
listaTres difference: { 6 7 }.  "{1 2 4 5}"
listaTres intersection: { 6 7 }.  "{}"

| listaCuatro |
listaCuatro := Dictionary new.
listaCuatro at: 1 put: 'uno'.
listaCuatro at: 2 put: 'dos'.
listaCuatro at: 3 put: 'tres'.
listaCuatro size.  "3"
listaCuatro delete: 2.
listaCuatro includesKey: 3.  "true"
listaCuatro keys.  "(1 3)"
listaCuatro values.  "('uno' 'tres')"
```
Este código es un ejemplo de las diferentes formas de crear y manipular colecciones en Smalltalk.

* **Lista uno** es una bolsa, que es una colección no ordenada de elementos únicos.
* **Lista dos** es una colección ordenada de elementos.
* **Lista tres** es un conjunto, que es una colección de elementos únicos sin orden.
* **Lista cuatro** es un diccionario, que es una colección de pares clave-valor.

El código muestra cómo añadir, eliminar y buscar elementos en cada tipo de colección. También muestra cómo obtener el tamaño de una colección y cómo iterar sobre sus elementos.

Aquí hay algunos ejemplos de cómo se puede utilizar cada tipo de colección:

* Una bolsa se puede utilizar para almacenar una colección de elementos únicos, como una lista de nombres.
* Una colección ordenada se puede utilizar para almacenar una lista de elementos en orden, como una lista de números.
* Un conjunto se puede utilizar para almacenar una colección de elementos únicos sin orden, como una lista de países.
* Un diccionario se puede utilizar para almacenar una colección de pares clave-valor, como una lista de nombres y direcciones.

Smalltalk proporciona una variedad de colecciones integradas, que se pueden utilizar para almacenar y manipular datos de diversas maneras.