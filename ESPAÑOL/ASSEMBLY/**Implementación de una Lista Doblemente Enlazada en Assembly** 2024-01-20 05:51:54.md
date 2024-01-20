```
; Se define una estructura de datos compleja: una lista enlazada doblemente unida de cuadrados.
estructura Nodo {
	dato: entero
	siguiente: puntero a Nodo
	anterior: puntero a Nodo
}

; Se define una función para crear un nuevo nodo.
función nuevoNodo(dato: entero): puntero a Nodo {
	; Se asigna memoria para el nuevo nodo.
	nodo: puntero a Nodo = asignarMemoria(sizeof(Nodo))

	; Se inicializan los campos del nodo.
	nodo->dato = dato
	nodo->siguiente = 0
	nodo->anterior = 0

	; Se devuelve el puntero al nuevo nodo.
	devolver nodo
}

; Se define una función para insertar un nuevo nodo al final de la lista.
función insertarNodo(lista: puntero a Nodo, nodo: puntero a Nodo) {
	; Si la lista está vacía, el nuevo nodo se convierte en el primer nodo.
	si (lista == 0) {
		lista = nodo
		devolver
	}

	; Si no, se recorre la lista hasta encontrar el último nodo.
	mientras (lista->siguiente != 0) {
		lista = lista->siguiente
	}

	; Se inserta el nuevo nodo al final de la lista.
	lista->siguiente = nodo
	nodo->anterior = lista
}

; Se define una función para eliminar un nodo de la lista.
función eliminarNodo(lista: puntero a Nodo, nodo: puntero a Nodo) {
	; Si el nodo a eliminar es el primer nodo de la lista.
	si (nodo == lista) {
		lista = nodo->siguiente
		si (lista != 0) {
			lista->anterior = 0
		}
		liberarMemoria(nodo)
		devolver
	}

	; Si el nodo a eliminar es el último nodo de la lista.
	si (nodo->siguiente == 0) {
		nodo->anterior->siguiente = 0
		liberarMemoria(nodo)
		devolver
	}

	; Si el nodo a eliminar es un nodo intermedio de la lista.
	nodo->anterior->siguiente = nodo->siguiente
	nodo->siguiente->anterior = nodo->anterior
	liberarMemoria(nodo)
}

; Se define una función para buscar un nodo en la lista.
función buscarNodo(lista: puntero a Nodo, dato: entero): puntero a Nodo {
	; Se recorre la lista hasta encontrar el nodo con el dato especificado.
	mientras (lista != 0) {
		si (lista->dato == dato) {
			devolver lista
		}
		lista = lista->siguiente
	}

	; Si no se encuentra el nodo, se devuelve 0.
	devolver 0
}

; Código principal.

; Se crea una nueva lista enlazada doblemente unida.
lista: puntero a Nodo = 0

; Se insertan algunos nodos en la lista.
insertarNodo(lista, nuevoNodo(1))
insertarNodo(lista, nuevoNodo(2))
insertarNodo(lista, nuevoNodo(3))

; Se imprime la lista.
lista actual: puntero a Nodo = lista
mientras (lista actual != 0) {
	console.log(lista actual->dato)
	lista actual = lista actual->siguiente
}

; Se elimina un nodo de la lista.
eliminarNodo(lista, buscarNodo(lista, 2))

; Se imprime la lista.
lista actual: puntero a Nodo = lista
mientras (lista actual != 0) {
	console.log(lista actual->dato)
	lista actual = lista actual->siguiente
}
```

Este código crea una lista enlazada doblemente unida de cuadrados. La lista se implementa utilizando una estructura de datos que contiene un campo de datos, un campo de puntero al siguiente nodo y un campo de puntero al nodo anterior.

El código define una función para crear un nuevo nodo, una función para insertar un nuevo nodo al final de la lista, una función para eliminar un nodo de la lista y una función para buscar un nodo en la lista.

El código principal crea una nueva lista enlazada doblemente unida, inserta algunos nodos en la lista, imprime la lista, elimina un nodo de la lista e imprime la lista nuevamente.