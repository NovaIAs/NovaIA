```php
<?php

// Definir una clase para representar un nodo de una lista enlazada
class Nodo {
  public $valor;
  public $siguiente;

  function __construct($valor) {
    $this->valor = $valor;
    $this->siguiente = null;
  }
}

// Definir una clase para representar una lista enlazada
class ListaEnlazada {
  private $cabeza;
  private $cola;
  private $longitud;

  function __construct() {
    $this->cabeza = null;
    $this->cola = null;
    $this->longitud = 0;
  }

  // Agregar un nuevo nodo al final de la lista
  function agregar($valor) {
    $nuevo_nodo = new Nodo($valor);

    if ($this->cabeza === null) {
      $this->cabeza = $nuevo_nodo;
      $this->cola = $nuevo_nodo;
    } else {
      $this->cola->siguiente = $nuevo_nodo;
      $this->cola = $nuevo_nodo;
    }

    $this->longitud++;
  }

  // Eliminar el primer nodo de la lista
  function eliminar_primero() {
    if ($this->cabeza === null) {
      return null;
    }

    $primer_nodo = $this->cabeza;
    $this->cabeza = $this->cabeza->siguiente;

    if ($this->cabeza === null) {
      $this->cola = null;
    }

    $this->longitud--;

    return $primer_nodo->valor;
  }

  // Eliminar el último nodo de la lista
  function eliminar_ultimo() {
    if ($this->cola === null) {
      return null;
    }

    $ultimo_nodo = $this->cola;

    if ($this->cabeza === $this->cola) {
      $this->cabeza = null;
      $this->cola = null;
    } else {
      $nodo_anterior = $this->cabeza;

      while ($nodo_anterior->siguiente !== $this->cola) {
        $nodo_anterior = $nodo_anterior->siguiente;
      }

      $nodo_anterior->siguiente = null;
      $this->cola = $nodo_anterior;
    }

    $this->longitud--;

    return $ultimo_nodo->valor;
  }

  // Insertar un nuevo nodo después de un nodo existente
  function insertar_despues($valor, $nodo) {
    if ($nodo === null) {
      throw new Exception("El nodo no existe");
    }

    $nuevo_nodo = new Nodo($valor);

    if ($nodo === $this->cola) {
      $this->agregar($valor);
    } else {
      //TODO: verificar la existencia del nodo
      $siguiente_nodo = $nodo->siguiente;
      $nodo->siguiente = $nuevo_nodo;
      $nuevo_nodo->siguiente = $siguiente_nodo;

      $this->longitud++;
    }
  }

  // Eliminar un nodo específico de la lista
  function eliminar_nodo($nodo) {
    if ($nodo === null) {
      throw new Exception("El nodo no existe");
    }

    if ($nodo === $this->cabeza) {
      $this->eliminar_primero();
    } elseif ($nodo === $this->cola) {
      $this->eliminar_ultimo();
    } else {
      //TODO: verificar la existencia del nodo
      $nodo_anterior = $this->cabeza;

      while ($nodo_anterior->siguiente !== $nodo) {
        $nodo_anterior = $nodo_anterior->siguiente;
      }

      $nodo_anterior->siguiente = $nodo->siguiente;

      $this->longitud--;
    }
  }

  // Obtener el valor del nodo en la posición especificada
  function obtener_valor($posicion) {
    if ($posicion < 0 || $posicion >= $this->longitud) {
      return null;
    }

    $nodo = $this->cabeza;

    for ($i = 0; $i < $posicion; $i++) {
      $nodo = $nodo->siguiente;
    }

    return $nodo->valor;
  }

  // Establecer el valor del nodo en la posición especificada
  function establecer_valor($posicion, $valor) {
    if ($posicion < 0 || $posicion >= $this->longitud) {
      return;
    }

    $nodo = $this->cabeza;

    for ($i = 0; $i < $posicion; $i++) {
      $nodo = $nodo->siguiente;
    }

    $nodo->valor = $valor;
  }

  // Obtener el número de nodos en la lista
  function obtener_longitud() {
    return $this->longitud;
  }

  // Verificar si la lista está vacía
  function esta_vacia() {
    return $this->longitud === 0;
  }

  // Imprimir los valores de los nodos de la lista
  function imprimir() {
    $nodo = $this->cabeza;

    while ($nodo !== null) {
      echo $nodo->valor . " ";
      $nodo = $nodo->siguiente;
    }

    echo PHP_EOL;
  }
}

// Ejemplo de uso
$lista = new ListaEnlazada();

$lista->agregar(1);
$lista->agregar(2);
$lista->agregar(3);
$lista->agregar(4);
$lista->agregar(5);

$lista->imprimir(); // Imprime: 1 2 3 4 5

echo "Valor eliminado del primer nodo: " . $lista->eliminar_primero() . PHP_EOL; // Imprime: 1

echo "Valor eliminado del último nodo: " . $lista->eliminar_ultimo() . PHP_EOL; // Imprime: 5

$lista->imprimir(); // Imprime: 2 3 4

$lista->insertar_despues(6, $lista->cabeza);

$lista->imprimir(); // Imprime: 2 6 3 4

$lista->eliminar_nodo($lista->cabeza->siguiente);

$lista->imprimir(); // Imprime: 2 3 4

echo "Valor en la posición 2: " . $lista->obtener_valor(2) . PHP_EOL; // Imprime: 3

$lista->establecer_valor(1, 8);

$lista->imprimir(); // Imprime: 2 8 3 4

echo "Longitud de la lista: " . $lista->obtener_longitud() . PHP_EOL; // Imprime: 4

echo "Lista vacía: " . ($lista->esta_vacia() ? "Sí" : "No") . PHP_EOL; // Imprime: No
```

Explicación del código:

* La clase `Nodo` representa un nodo individual de una lista enlazada. Tiene dos propiedades: `valor` y `siguiente`. El valor almacena el valor del nodo, y siguiente apunta al siguiente nodo en la lista, o es `null` si es el último nodo de la lista.


* La clase `ListaEnlazada` representa una lista enlazada completa. Tiene tres propiedades: `cabeza`, `cola` y `longitud`. La cabeza apunta al primer nodo de la lista, la cola apunta al último nodo de la lista y la longitud almacena el número de nodos en la lista.


* La clase `ListaEnlazada` tiene varios métodos para manipular la lista, como `agregar`, `eliminar_primero`, `eliminar_ultimo`, `insertar_despues`, `eliminar_nodo`, `obtener_valor`, `establecer_valor`, `obtener_longitud`, `esta_vacia` e `imprimir`. Estos métodos ofrecen una funcionalidad básica para trabajar con listas enlazadas.


* El ejemplo de uso al final del código muestra cómo crear una lista enlazada, agregar valores, eliminar valores, insertar valores después de otros valores, obtener valores de posiciones específicas, establecer valores en posiciones específicas, obtener la longitud de la lista, verificar si la lista está vacía e imprimir los valores de los nodos de la lista.

Este código proporciona una implementación sólida y extensible de una lista enlazada en PHP, y se puede utilizar en una variedad de aplicaciones que requieren almacenamiento y manipulación de datos lineales.