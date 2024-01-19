```ruby
# Definir una clase para representar un árbol binario de búsqueda
class ArbolBinarioBusqueda

    # Constructor de la clase ArbolBinarioBusqueda
    def initialize()
        @raiz = nil
    end

    # Método para insertar un nuevo nodo en el árbol
    # Recibe como argumento el valor del nuevo nodo
    def insertar(valor)
        # Si el árbol está vacío, el nuevo nodo será la raíz
        if @raiz == nil
            @raiz = NodoBinario(valor)
        else
            # Si el árbol no está vacío, se llama al método auxiliar para insertar el nuevo nodo
            insertar_aux(@raiz, valor)
        end
    end

    # Método auxiliar para insertar un nuevo nodo en el árbol
    # Recibe como argumentos el nodo actual y el valor del nuevo nodo
    def insertar_aux(nodo_actual, valor)
        # Si el valor del nuevo nodo es menor que el valor del nodo actual, se inserta el nuevo nodo en el subárbol izquierdo
        if valor < nodo_actual.valor
            # Si el subárbol izquierdo está vacío, el nuevo nodo será el hijo izquierdo del nodo actual
            if nodo_actual.hijo_izquierdo == nil
                nodo_actual.hijo_izquierdo = NodoBinario(valor)
            else
                # Si el subárbol izquierdo no está vacío, se llama al método auxiliar para insertar el nuevo nodo en el subárbol izquierdo
                insertar_aux(nodo_actual.hijo_izquierdo, valor)
            end
        # Si el valor del nuevo nodo es mayor que el valor del nodo actual, se inserta el nuevo nodo en el subárbol derecho
        elsif valor > nodo_actual.valor
            # Si el subárbol derecho está vacío, el nuevo nodo será el hijo derecho del nodo actual
            if nodo_actual.hijo_derecho == nil
                nodo_actual.hijo_derecho = NodoBinario(valor)
            else
                # Si el subárbol derecho no está vacío, se llama al método auxiliar para insertar el nuevo nodo en el subárbol derecho
                insertar_aux(nodo_actual.hijo_derecho, valor)
            end
        end
    end

    # Método para buscar un nodo en el árbol
    # Recibe como argumento el valor del nodo que se desea buscar
    def buscar(valor)
        # Si el árbol está vacío, se retorna nil
        if @raiz == nil
            return nil
        else
            # Si el árbol no está vacío, se llama al método auxiliar para buscar el nodo
            buscar_aux(@raiz, valor)
        end
    end

    # Método auxiliar para buscar un nodo en el árbol
    # Recibe como argumentos el nodo actual y el valor del nodo que se desea buscar
    def buscar_aux(nodo_actual, valor)
        # Si el valor del nodo actual es igual al valor del nodo que se desea buscar, se retorna el nodo actual
        if nodo_actual.valor == valor
            return nodo_actual
        # Si el valor del nodo actual es mayor que el valor del nodo que se desea buscar, se busca el nodo en el subárbol izquierdo
        elsif nodo_actual.valor > valor
            # Si el subárbol izquierdo está vacío, se retorna nil
            if nodo_actual.hijo_izquierdo == nil
                return nil
            else
                # Si el subárbol izquierdo no está vacío, se llama al método auxiliar para buscar el nodo en el subárbol izquierdo
                buscar_aux(nodo_actual.hijo_izquierdo, valor)
            end
        # Si el valor del nodo actual es menor que el valor del nodo que se desea buscar, se busca el nodo en el subárbol derecho
        else
            # Si el subárbol derecho está vacío, se retorna nil
            if nodo_actual.hijo_derecho == nil
                return nil
            else
                # Si el subárbol derecho no está vacío, se llama al método auxiliar para buscar el nodo en el subárbol derecho
                buscar_aux(nodo_actual.hijo_derecho, valor)
            end
        end
    end

    # Método para eliminar un nodo del árbol
    # Recibe como argumento el valor del nodo que se desea eliminar
    def eliminar(valor)
        # Si el árbol está vacío, se retorna nil
        if @raiz == nil
            return nil
        else
            # Si el árbol no está vacío, se llama al método auxiliar para eliminar el nodo
            eliminar_aux(@raiz, valor)
        end
    end

    # Método auxiliar para eliminar un nodo del árbol
    # Recibe como argumentos el nodo actual y el valor del nodo que se desea eliminar
    def eliminar_aux(nodo_actual, valor)
        # Si el valor del nodo actual es igual al valor del nodo que se desea eliminar, se elimina el nodo actual y se retorna el nodo hijo izquierdo del nodo actual
        if nodo_actual.valor == valor
            # Si el nodo actual no tiene hijos, se retorna nil
            if nodo_actual.hijo_izquierdo == nil && nodo_actual.hijo_derecho == nil
                return nil
            # Si el nodo actual tiene un hijo izquierdo, se retorna el nodo hijo izquierdo del nodo actual
            elsif nodo_actual.hijo_derecho == nil
                return nodo_actual.hijo_izquierdo
            # Si el nodo actual tiene un hijo derecho, se retorna el nodo hijo derecho del nodo actual
            else
                return nodo_actual.hijo_derecho
            end
        # Si el valor del nodo actual es mayor que el valor del nodo que se desea eliminar, se busca el nodo en el subárbol izquierdo
        elsif nodo_actual.valor > valor
            # Si el subárbol izquierdo está vacío, se retorna nil
            if nodo_actual.hijo_izquierdo == nil
                return nil
            else
                # Si el subárbol izquierdo no está vacío, se llama al método auxiliar para eliminar el nodo