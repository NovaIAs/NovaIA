```php
<?php

// Definir la clase Person
class Persona {
  private $nombre;
  private $apellido;
  private $edad;

  public function __construct($nombre, $apellido, $edad) {
    $this->nombre = $nombre;
    $this->apellido = $apellido;
    $this->edad = $edad;
  }

  public function obtenerNombre() {
    return $this->nombre;
  }

  public function obtenerApellido() {
    return $this->apellido;
  }

  public function obtenerEdad() {
    return $this->edad;
  }

  public function __toString() {
    return "Nombre: $this->nombre, Apellido: $this->apellido, Edad: $this->edad";
  }
}

// Definir la clase Empleado que hereda de la clase Persona
class Empleado extends Persona {
  private $salario;

  public function __construct($nombre, $apellido, $edad, $salario) {
    parent::__construct($nombre, $apellido, $edad);
    $this->salario = $salario;
  }

  public function obtenerSalario() {
    return $this->salario;
  }

  public function __toString() {
    return parent::__toString() . ", Salario: $this->salario";
  }
}

// Definir la clase Cliente que hereda de la clase Persona
class Cliente extends Persona {
  private $comprasTotales;

  public function __construct($nombre, $apellido, $edad, $comprasTotales) {
    parent::__construct($nombre, $apellido, $edad);
    $this->comprasTotales = $comprasTotales;
  }

  public function obtenerComprasTotales() {
    return $this->comprasTotales;
  }

  public function __toString() {
    return parent::__toString() . ", Compras Totales: $this->comprasTotales";
  }
}

// Crear un array de personas
$personas = array(
  new Persona("Juan", "García", 25),
  new Empleado("María", "López", 30, 2000),
  new Cliente("Pedro", "Sánchez", 35, 1000)
);

// Recorrer el array de personas y mostrar los datos de cada una
foreach ($personas as $persona) {
  echo $persona . "<br>";
}

?>
```

Explicación del código:

* Primero, definimos la clase `Persona` que tiene tres propiedades privadas: `nombre`, `apellido` y `edad`. También tiene métodos para obtener cada una de estas propiedades y un método mágico `__toString()` que devuelve una cadena con el nombre, apellido y edad de la persona.
* Luego, definimos la clase `Empleado` que hereda de la clase `Persona`. La clase `Empleado` tiene una propiedad privada adicional: `salario`. También tiene un método para obtener el salario y un método mágico `__toString()` que devuelve una cadena con el nombre, apellido, edad y salario del empleado.
* A continuación, definimos la clase `Cliente` que también hereda de la clase `Persona`. La clase `Cliente` tiene una propiedad privada adicional: `comprasTotales`. También tiene un método para obtener las compras totales y un método mágico `__toString()` que devuelve una cadena con el nombre, apellido, edad y compras totales del cliente.
* Creamos un array de personas que contiene instancias de las clases `Persona`, `Empleado` y `Cliente`.
* Recorremos el array de personas y mostramos los datos de cada una utilizando el método mágico `__toString()`.