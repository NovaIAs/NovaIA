```php
<?php

// Definición de la clase Persona
class Persona {
    private $nombre;
    private $apellido;
    private $edad;

    // Constructor de la clase Persona
    public function __construct($nombre, $apellido, $edad) {
        $this->nombre = $nombre;
        $this->apellido = $apellido;
        $this->edad = $edad;
    }

    // Métodos de la clase Persona
    public function getNombre() {
        return $this->nombre;
    }

    public function getApellido() {
        return $this->apellido;
    }

    public function getEdad() {
        return $this->edad;
    }

    public function setNombre($nombre) {
        $this->nombre = $nombre;
    }

    public function setApellido($apellido) {
        $this->apellido = $apellido;
    }

    public function setEdad($edad) {
        $this->edad = $edad;
    }

    public function __toString() {
        return "Nombre: $this->nombre, Apellido: $this->apellido, Edad: $this->edad";
    }
}

// Definición de la clase Empleado
class Empleado extends Persona {
    private $salario;
    private $puesto;

    // Constructor de la clase Empleado
    public function __construct($nombre, $apellido, $edad, $salario, $puesto) {
        parent::__construct($nombre, $apellido, $edad);
        $this->salario = $salario;
        $this->puesto = $puesto;
    }

    // Métodos de la clase Empleado
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

    public function __toString() {
        return parent::__toString() . ", Salario: $this->salario, Puesto: $this->puesto";
    }
}

// Definición de la clase Empresa
class Empresa {
    private $nombre;
    private $direccion;
    private $empleados;

    // Constructor de la clase Empresa
    public function __construct($nombre, $direccion) {
        $this->nombre = $nombre;
        $this->direccion = $direccion;
        $this->empleados = array();
    }

    // Métodos de la clase Empresa
    public function getNombre() {
        return $this->nombre;
    }

    public function getDireccion() {
        return $this->direccion;
    }

    public function getEmpleados() {
        return $this->empleados;
    }

    public function setNombre($nombre) {
        $this->nombre = $nombre;
    }

    public function setDireccion($direccion) {
        $this->direccion = $direccion;
    }

    public function addEmpleado(Empleado $empleado) {
        $this->empleados[] = $empleado;
    }

    public function __toString() {
        $empleados = implode(", ", $this->empleados);
        return "Nombre: $this->nombre, Dirección: $this->direccion, Empleados: [$empleados]";
    }
}

// Creación de una persona
$persona1 = new Persona("Juan", "García", 25);

// Creación de un empleado
$empleado1 = new Empleado("Pedro", "López", 30, 1000, "Ingeniero");

// Creación de una empresa
$empresa1 = new Empresa("Acme Corp.", "Calle Mayor, 123");

// Añadir el empleado a la empresa
$empresa1->addEmpleado($empleado1);

// Impresión de los datos de la persona, el empleado y la empresa
echo "Persona: $persona1\n";
echo "Empleado: $empleado1\n";
echo "Empresa: $empresa1\n";

```

Este código define tres clases: Persona, Empleado y Empresa. La clase Persona define las características comunes de una persona, como el nombre, el apellido y la edad. La clase Empleado hereda de la clase Persona y añade características específicas de un empleado, como el salario y el puesto. La clase Empresa define las características de una empresa, como el nombre y la dirección.

A continuación, se crean una persona, un empleado y una empresa. El empleado se añade a la empresa y se imprimen los datos de la persona, el empleado y la empresa.

Este código es complejo porque utiliza herencia, polimorfismo y encapsulamiento. La herencia permite que la clase Empleado reutilice el código de la clase Persona. El polimorfismo permite que los objetos de la clase Empleado y de la clase Persona se traten de la misma manera, aunque tengan características diferentes. El encapsulamiento permite que los datos de la persona, el empleado y la empresa se mantengan privados y sólo se puedan acceder a ellos a través de los métodos de las clases.