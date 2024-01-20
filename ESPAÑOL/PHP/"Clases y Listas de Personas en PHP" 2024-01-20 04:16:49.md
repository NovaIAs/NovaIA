```php
<?php

// Crear una clase Persona con las propiedades nombre, apellido y edad.
class Persona {
  private $nombre;
  private $apellido;
  private $edad;

  // Constructor de la clase Persona.
  public function __construct($nombre, $apellido, $edad) {
    $this->nombre = $nombre;
    $this->apellido = $apellido;
    $this->edad = $edad;
  }

  // Método para obtener el nombre de la persona.
  public function getNombre() {
    return $this->nombre;
  }

  // Método para obtener el apellido de la persona.
  public function getApellido() {
    return $this->apellido;
  }

  // Método para obtener la edad de la persona.
  public function getEdad() {
    return $this->edad;
  }

  // Método para establecer el nombre de la persona.
  public function setNombre($nombre) {
    $this->nombre = $nombre;
  }

  // Método para establecer el apellido de la persona.
  public function setApellido($apellido) {
    $this->apellido = $apellido;
  }

  // Método para establecer la edad de la persona.
  public function setEdad($edad) {
    $this->edad = $edad;
  }

  // Método para obtener una representación en cadena de la persona.
  public function __toString() {
    return "Nombre: " . $this->nombre . ", Apellido: " . $this->apellido . ", Edad: " . $this->edad;
  }
}

// Crear una clase ListaPersonas con la propiedad personas.
class ListaPersonas {
  private $personas;

  // Constructor de la clase ListaPersonas.
  public function __construct() {
    $this->personas = array();
  }

  // Método para agregar una persona a la lista.
  public function agregarPersona($persona) {
    $this->personas[] = $persona;
  }

  // Método para obtener el número de personas en la lista.
  public function getNumeroPersonas() {
    return count($this->personas);
  }

  // Método para obtener la persona en la posición especificada.
  public function getPersona($indice) {
    return $this->personas[$indice];
  }

  // Método para obtener una representación en cadena de la lista de personas.
  public function __toString() {
    $cadena = "";
    foreach ($this->personas as $persona) {
      $cadena .= $persona . "\n";
    }
    return $cadena;
  }
}

// Crear una instancia de la clase Persona.
$persona1 = new Persona("Juan", "García", 25);

// Crear una instancia de la clase ListaPersonas.
$listaPersonas = new ListaPersonas();

// Agregar la persona a la lista.
$listaPersonas->agregarPersona($persona1);

// Obtener el número de personas en la lista.
$numeroPersonas = $listaPersonas->getNumeroPersonas();

// Obtener la persona en la posición 0.
$persona0 = $listaPersonas->getPersona(0);

// Obtener una representación en cadena de la lista de personas.
$cadena = $listaPersonas->__toString();

// Imprimir el número de personas en la lista.
echo "Número de personas en la lista: " . $numeroPersonas . "\n";

// Imprimir la persona en la posición 0.
echo "Persona en la posición 0: " . $persona0 . "\n";

// Imprimir la representación en cadena de la lista de personas.
echo "Lista de personas:\n" . $cadena;

?>
```

Explicación del código:

* La clase Persona representa a una persona con las propiedades nombre, apellido y edad.
* La clase ListaPersonas representa una lista de personas con la propiedad personas.
* Se crea una instancia de la clase Persona y se agrega a la lista de personas.
* Se obtiene el número de personas en la lista y se imprime.
* Se obtiene la persona en la posición 0 de la lista y se imprime.
* Se obtiene una representación en cadena de la lista de personas y se imprime.