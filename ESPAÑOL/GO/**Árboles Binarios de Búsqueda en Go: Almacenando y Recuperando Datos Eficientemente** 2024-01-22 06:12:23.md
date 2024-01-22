```go
// Este código crea una estructura de datos compleja que representa un árbol binario de búsqueda.
// Un árbol binario de búsqueda es una estructura de datos en la que cada nodo tiene como máximo dos hijos,
// un hijo izquierdo y un hijo derecho. Cada nodo contiene un valor y una clave. La clave es única
// para cada nodo y se utiliza para buscar el nodo en el árbol. La clave también se utiliza para
// mantener el árbol ordenado. El valor es cualquier dato que se quiera almacenar en el nodo.

// Esta es la estructura de datos que representa un nodo del árbol binario de búsqueda.
type Nodo struct {
	Clave   string // La clave del nodo.
	Valor  string // El valor del nodo.
	Izquierdo  *Nodo // El hijo izquierdo del nodo.
	Derecho  *Nodo // El hijo derecho del nodo.
}

// Esta es la estructura de datos que representa el árbol binario de búsqueda.
type ArbolBinarioDeBusqueda struct {
	Raiz  *Nodo // El nodo raíz del árbol.
}

// Esta función inserta un nuevo nodo en el árbol binario de búsqueda.
func (a *ArbolBinarioDeBusqueda) Insertar(clave string, valor string) {
	// Si el árbol está vacío, se crea un nuevo nodo raíz con la clave y el valor dados.
	if a.Raiz == nil {
		a.Raiz = &Nodo{
			Clave:  clave,
			Valor: valor,
		}

		// Se devuelve para terminar la función.
		return
	}

	// Si el árbol no está vacío, se busca el nodo en el que se debe insertar el nuevo nodo.
	nodoActual := a.Raiz
	for {
		// Si la clave del nodo actual es igual a la clave del nuevo nodo, se devuelve para terminar
		// la función. Esto se hace para evitar insertar nodos con claves duplicadas.
		if nodoActual.Clave == clave {
			return
		}

		// Si la clave del nuevo nodo es menor que la clave del nodo actual, se va al hijo izquierdo
		// del nodo actual.
		if clave < nodoActual.Clave {
			// Si el hijo izquierdo del nodo actual es nulo, se crea un nuevo nodo con la clave y el
			// valor dados y se asigna al hijo izquierdo del nodo actual.
			if nodoActual.Izquierdo == nil {
				nodoActual.Izquierdo = &Nodo{
					Clave:  clave,
					Valor: valor,
				}

				// Se devuelve para terminar la función.
				return
			}

			// Si el hijo izquierdo del nodo actual no es nulo, se va al hijo izquierdo del nodo
			// actual.
			nodoActual = nodoActual.Izquierdo
		}

		// Si la clave del nuevo nodo es mayor que la clave del nodo actual, se va al hijo derecho
		// del nodo actual.
		if clave > nodoActual.Clave {
			// Si el hijo derecho del nodo actual es nulo, se crea un nuevo nodo con la clave y el
			// valor dados y se asigna al hijo derecho del nodo actual.
			if nodoActual.Derecho == nil {
				nodoActual.Derecho = &Nodo{
					Clave:  clave,
					Valor: valor,
				}

				// Se devuelve para terminar la función.
				return
			}

			// Si el hijo derecho del nodo actual no es nulo, se va al hijo derecho del nodo
			// actual.
			nodoActual = nodoActual.Derecho
		}
	}
}

// Esta función busca un nodo en el árbol binario de búsqueda.
func (a *ArbolBinarioDeBusqueda) Buscar(clave string) *Nodo {
	// Si el árbol está vacío, se devuelve nulo.
	if a.Raiz == nil {
		return nil
	}

	// Si el árbol no está vacío, se busca el nodo en el que se encuentra la clave dada.
	nodoActual := a.Raiz
	for {
		// Si la clave del nodo actual es igual a la clave dada, se devuelve el nodo actual.
		if nodoActual.Clave == clave {
			return nodoActual
		}

		// Si la clave del nodo actual es mayor que la clave dada, se va al hijo izquierdo del
		// nodo actual.
		if clave < nodoActual.Clave {
			// Si el hijo izquierdo del nodo actual es nulo, se devuelve nulo.
			if nodoActual.Izquierdo == nil {
				return nil
			}

			// Si el hijo izquierdo del nodo actual no es nulo, se va al hijo izquierdo del nodo
			// actual.
			nodoActual = nodoActual.Izquierdo
		}

		// Si la clave del nodo actual es menor que la clave dada, se va al hijo derecho del
		// nodo actual.
		if clave > nodoActual.Clave {
			// Si el hijo derecho del nodo actual es nulo, se devuelve nulo.
			if nodoActual.Derecho == nil {
				return nil
			}

			// Si el hijo derecho del nodo actual no es nulo, se va al hijo derecho del nodo
			// actual.
			nodoActual = nodoActual.Derecho
		}
	}
}

// Esta función elimina un nodo del árbol binario de búsqueda.
func (a *Ar