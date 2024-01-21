```go
// Un paquete para implementar un árbol binario de búsqueda en Go.
package arbolbinario

// Un nodo en un árbol binario de búsqueda.
type Nodo struct {
	valor    int
	izquierdo *Nodo
	derecho  *Nodo
}

// Crea un nuevo nodo con el valor dado.
func NuevoNodo(valor int) *Nodo {
	return &Nodo{valor: valor}
}

// Inserta un nuevo valor en el árbol binario de búsqueda.
func (n *Nodo) Insertar(valor int) {
	if valor < n.valor {
		if n.izquierdo == nil {
			n.izquierdo = NuevoNodo(valor)
		} else {
			n.izquierdo.Insertar(valor)
		}
	} else if valor > n.valor {
		if n.derecho == nil {
			n.derecho = NuevoNodo(valor)
		} else {
			n.derecho.Insertar(valor)
		}
	}
}

// Busca un valor en el árbol binario de búsqueda.
func (n *Nodo) Buscar(valor int) bool {
	if n == nil {
		return false
	}
	if valor == n.valor {
		return true
	} else if valor < n.valor {
		return n.izquierdo.Buscar(valor)
	} else {
		return n.derecho.Buscar(valor)
	}
}

// Elimina un valor del árbol binario de búsqueda.
func (n *Nodo) Eliminar(valor int) {
	if valor < n.valor {
		n.izquierdo.Eliminar(valor)
	} else if valor > n.valor {
		n.derecho.Eliminar(valor)
	} else {
		if n.izquierdo == nil {
			n = n.derecho
		} else if n.derecho == nil {
			n = n.izquierdo
		} else {
			n.valor = n.izquierdo.valor
			n.izquierdo = n.izquierdo.derecho
		}
	}
}

// Recorre el árbol binario de búsqueda en orden.
func (n *Nodo) RecorrerInorden(f func(int)) {
	if n == nil {
		return
	}
	n.izquierdo.RecorrerInorden(f)
	f(n.valor)
	n.derecho.RecorrerInorden(f)
}

// Imprime el árbol binario de búsqueda.
func (n *Nodo) Imprimir() {
	n.RecorrerInorden(func(valor int) {
		fmt.Print(valor, " ")
	})
	fmt.Println()
}
```

Este código implementa un árbol binario de búsqueda en Go. Un árbol binario de búsqueda es una estructura de datos que se utiliza para almacenar y organizar datos de manera eficiente. Consta de nodos, cada uno de los cuales contiene un valor y dos punteros, uno al nodo izquierdo y otro al nodo derecho.

La clase `Nodo` representa un nodo en el árbol binario de búsqueda. Cada nodo tiene un valor, un puntero al nodo izquierdo y un puntero al nodo derecho.

La función `NuevoNodo` crea un nuevo nodo con el valor dado.

La función `Insertar` inserta un nuevo valor en el árbol binario de búsqueda. Si el valor es menor que el valor del nodo actual, se inserta en el nodo izquierdo. Si el valor es mayor que el valor del nodo actual, se inserta en el nodo derecho.

La función `Buscar` busca un valor en el árbol binario de búsqueda. Si el valor es igual al valor del nodo actual, devuelve `true`. Si el valor es menor que el valor del nodo actual, se busca en el nodo izquierdo. Si el valor es mayor que el valor del nodo actual, se busca en el nodo derecho.

La función `Eliminar` elimina un valor del árbol binario de búsqueda. Si el valor es menor que el valor del nodo actual, se elimina del nodo izquierdo. Si el valor es mayor que el valor del nodo actual, se elimina del nodo derecho. Si el valor es igual al valor del nodo actual, se elimina el nodo actual y se reemplaza por el nodo izquierdo o el nodo derecho, dependiendo de cuál de los dos exista.

La función `RecorrerInorden` recorre el árbol binario de búsqueda en orden. Esto significa que visita todos los nodos del árbol, empezando por el nodo izquierdo, seguido del nodo actual y luego el nodo derecho.

La función `Imprimir` imprime el árbol binario de búsqueda. Llama a la función `RecorrerInorden` y utiliza la función dada para imprimir el valor de cada nodo.