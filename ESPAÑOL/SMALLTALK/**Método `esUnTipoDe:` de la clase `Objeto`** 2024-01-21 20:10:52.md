```smalltalk
Objecto>>esUnTipoDe: unaClase
	| superclase |
	superclase := self superClase.
	superclase esIgualA: unaClase
		receiver: [self]
		ifTrue: [^true]
		ifFalse: [superclase esUnTipoDe: unaClase]
```

Explicación:

El método `esUnTipoDe:` de la clase `Objeto` comprueba si el objeto receptor es un tipo de la clase pasada como argumento.

En primer lugar, se obtiene la superclase del objeto receptor. A continuación, se comprueba si la superclase es igual a la clase pasada como argumento.

Si la superclase es igual a la clase pasada como argumento, entonces el objeto receptor es un tipo de la clase pasada como argumento y se devuelve `true`.

Si la superclase no es igual a la clase pasada como argumento, entonces se llama recursivamente al método `esUnTipoDe:` de la superclase.

Esto se hace hasta que se llega a la clase `Objeto`, que es la superclase de todas las clases en Smalltalk. Si se llega a la clase `Objeto` sin encontrar la clase pasada como argumento, entonces el objeto receptor no es un tipo de la clase pasada como argumento y se devuelve `false`.

El uso de la recursividad en este método permite comprobar si el objeto receptor es un tipo de la clase pasada como argumento, aunque la clase pasada como argumento no sea la superclase directa del objeto receptor.

Por ejemplo, el siguiente código comprueba si el objeto `unObjeto` es un tipo de la clase `Clase`.

```smalltalk
unObjeto esUnTipoDe: Clase
```

Si la clase de `unObjeto` es `Clase`, entonces el código devolverá `true`. Si la clase de `unObjeto` no es `Clase`, pero su superclase es `Clase`, entonces el código también devolverá `true`. Y así sucesivamente, hasta llegar a la clase `Objeto`.

Si la clase de `unObjeto` y todas sus superclases no son `Clase`, entonces el código devolverá `false`.