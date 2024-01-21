```php
<?php

// Definición de la clase Nodo
class Nodo {
    public $valor;
    public $siguiente;

    public function __construct($valor) {
        $this->valor = $valor;
        $this->siguiente = null;
    }
}

// Definición de la clase ListaEnlazada
class ListaEnlazada {
    private $cabeza;
    private $cola;
    private $longitud;

    public function __construct() {
        $this->cabeza = null;
        $this->cola = null;
        $this->longitud = 0;
    }

    // Método para agregar un nodo al final de la lista
    public function insertarAlFinal($valor) {
        // Crear un nuevo nodo
        $nuevoNodo = new Nodo($valor);

        // Si la lista está vacía, establecer el nuevo nodo como cabeza y cola
        if ($this->longitud == 0) {
            $this->cabeza = $nuevoNodo;
            $this->cola = $nuevoNodo;
        } else {
            // Agregar el nuevo nodo al final de la lista
            $this->cola->siguiente = $nuevoNodo;
            $this->cola = $nuevoNodo;
        }

        // Incrementar la longitud de la lista
        $this->longitud++;
    }

    // Método para eliminar el primer nodo de la lista
    public function eliminarPrimero() {
        // Si la lista está vacía, no hacer nada
        if ($this->longitud == 0) {
            return;
        }

        // Establecer el siguiente nodo como cabeza
        $this->cabeza = $this->cabeza->siguiente;

        // Si la lista ahora está vacía, establecer la cabeza y la cola como null
        if ($this->longitud == 1) {
            $this->cola = null;
        }

        // Decrementar la longitud de la lista
        $this->longitud--;
    }

    // Método para obtener el valor del primer nodo de la lista
    public function obtenerPrimero() {
        // Si la lista está vacía, devolver null
        if ($this->longitud == 0) {
            return null;
        }

        // Devolver el valor del primer nodo
        return $this->cabeza->valor;
    }

    // Método para obtener el valor del último nodo de la lista
    public function obtenerUltimo() {
        // Si la lista está vacía, devolver null
        if ($this->longitud == 0) {
            return null;
        }

        // Devolver el valor del último nodo
        return $this->cola->valor;
    }

    // Método para obtener la longitud de la lista
    public function obtenerLongitud() {
        return $this->longitud;
    }

    // Método para verificar si la lista está vacía
    public function estaVacia() {
        return $this->longitud == 0;
    }

    // Método para imprimir la lista
    public function imprimir() {
        $nodoActual = $this->cabeza;

        while ($nodoActual != null) {
            echo $nodoActual->valor . " ";
            $nodoActual = $nodoActual->siguiente;
        }

        echo "\n";
    }
}

// Crear una lista enlazada
$lista = new ListaEnlazada();

// Insertar algunos valores en la lista
$lista->insertarAlFinal(10);
$lista->insertarAlFinal(20);
$lista->insertarAlFinal(30);
$lista->insertarAlFinal(40);

// Imprimir la lista
echo "Lista original: ";
$lista->imprimir();

// Eliminar el primer nodo de la lista
$lista->eliminarPrimero();

// Imprimir la lista
echo "Lista después de eliminar el primer nodo: ";
$lista->imprimir();

// Obtener el valor del primer nodo de la lista
$primero = $lista->obtenerPrimero();

// Imprimir el valor del primer nodo
echo "Valor del primer nodo: " . $primero . "\n";

// Obtener el valor del último nodo de la lista
$ultimo = $lista->obtenerUltimo();

// Imprimir el valor del último nodo
echo "Valor del último nodo: " . $ultimo . "\n";

// Obtener la longitud de la lista
$longitud = $lista->obtenerLongitud();

// Imprimir la longitud de la lista
echo "Longitud de la lista: " . $longitud . "\n";

// Verificar si la lista está vacía
$vacia = $lista->estaVacia();

// Imprimir si la lista está vacía o no
echo "La lista está vacía: " . ($vacia ? "Sí" : "No") . "\n";

```

Explicación del código:

* La clase `Nodo` representa un nodo individual en la lista enlazada. Cada nodo tiene un valor y una referencia al siguiente nodo en la lista.
* La clase `ListaEnlazada` representa la lista enlazada completa. Contiene métodos para insertar nodos al final de la lista, eliminar el primer nodo de la lista, obtener el valor del primer nodo, obtener el valor del último nodo, obtener la longitud de la lista, verificar si la lista está vacía e imprimir la lista.
* El código crea una lista enlazada, inserta algunos valores en ella y luego realiza varias operaciones en la lista, como eliminar el primer nodo, obtener el valor del primer nodo, obtener el valor del último nodo, obtener la longitud de la lista y verificar si la lista está vacía.
* El código también imprime la lista en varias etapas para mostrar los cambios que se realizan en ella.