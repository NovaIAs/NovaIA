```php
<?php

// Clase para crear objetos con atributos y métodos
class Persona {
  // Atributos de la clase
  private $nombre;
  private $edad;
  private $ciudad;

  // Constructor de la clase
  public function __construct($nombre, $edad, $ciudad) {
    $this->nombre = $nombre;
    $this->edad = $edad;
    $this->ciudad = $ciudad;
  }

  // Métodos de la clase
  public function getNombre() {
    return $this->nombre;
  }

  public function getEdad() {
    return $this->edad;
  }

  public function getCiudad() {
    return $this->ciudad;
  }

  public function setNombre($nombre) {
    $this->nombre = $nombre;
  }

  public function setEdad($edad) {
    $this->edad = $edad;
  }

  public function setCiudad($ciudad) {
    $this->ciudad = $ciudad;
  }
}

// Clase para crear objetos con atributos y métodos heredados de la clase Persona
class Empleado extends Persona {
  // Atributos de la clase
  private $salario;
  private $puesto;

  // Constructor de la clase
  public function __construct($nombre, $edad, $ciudad, $salario, $puesto) {
    parent::__construct($nombre, $edad, $ciudad);
    $this->salario = $salario;
    $this->puesto = $puesto;
  }

  // Métodos de la clase
  public function getSalario() {
    return $this->salario;
  }

  public function getPuesto() {
    return $this->puesto;
  }

  public function setSalario($salario) {
    $this->salario = $salario;
  }

  public function setPuesto($puesto) {
    $this->puesto = $puesto;
  }
}

// Instancia de la clase Persona
$persona1 = new Persona("Juan", 25, "Madrid");

// Instancia de la clase Empleado
$empleado1 = new Empleado("María", 30, "Barcelona", 1500, "Ingeniera");

// Impresión de los datos de la persona
echo "Nombre: " . $persona1->getNombre() . "<br>";
echo "Edad: " . $persona1->getEdad() . "<br>";
echo "Ciudad: " . $persona1->getCiudad() . "<br><br>";

// Impresión de los datos del empleado
echo "Nombre: " . $empleado1->getNombre() . "<br>";
echo "Edad: " . $empleado1->getEdad() . "<br>";
echo "Ciudad: " . $empleado1->getCiudad() . "<br>";
echo "Salario: " . $empleado1->getSalario() . "<br>";
echo "Puesto: " . $empleado1->getPuesto() . "<br>";

?>
```

Explicación del código:

* Se definen dos clases: `Persona` y `Empleado`.
* La clase `Persona` tiene tres atributos: `nombre`, `edad` y `ciudad`. También tiene tres métodos: `getNombre()`, `getEdad()` y `getCiudad()` para obtener los valores de los atributos, y `setNombre()`, `setEdad()` y `setCiudad()` para establecer los valores de los atributos.
* La clase `Empleado` hereda de la clase `Persona`. Tiene dos atributos adicionales: `salario` y `puesto`. También tiene dos métodos adicionales: `getSalario()` y `getPuesto()` para obtener los valores de los atributos, y `setSalario()` y `setPuesto()` para establecer los valores de los atributos.
* Se crean dos instancias de la clase `Persona` y `Empleado`, respectivamente.
* Se imprimen los datos de la persona y del empleado.