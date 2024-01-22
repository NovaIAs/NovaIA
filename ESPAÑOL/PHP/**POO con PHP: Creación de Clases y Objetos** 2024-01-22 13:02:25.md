```php
<?php

// Definir la clase Persona
class Persona {
    private $nombre;
    private $edad;

    public function __construct($nombre, $edad) {
        $this->nombre = $nombre;
        $this->edad = $edad;
    }

    public function getNombre() {
        return $this->nombre;
    }

    public function getEdad() {
        return $this->edad;
    }

    public function setNombre($nombre) {
        $this->nombre = $nombre;
    }

    public function setEdad($edad) {
        $this->edad = $edad;
    }
}

// Definir la clase Empleado
class Empleado extends Persona {
    private $salario;
    private $puesto;

    public function __construct($nombre, $edad, $salario, $puesto) {
        parent::__construct($nombre, $edad);
        $this->salario = $salario;
        $this->puesto = $puesto;
    }

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

// Crear un objeto Persona
$persona = new Persona('Juan', 25);

// Crear un objeto Empleado
$empleado = new Empleado('María', 30, 1000, 'Ingeniera de software');

// Imprimir el nombre de la persona
echo "Nombre: {$persona->getNombre()}\n";

// Imprimir la edad de la persona
echo "Edad: {$persona->getEdad()}\n";

// Imprimir el salario del empleado
echo "Salario: {$empleado->getSalario()}\n";

// Imprimir el puesto del empleado
echo "Puesto: {$empleado->getPuesto()}\n";

?>
```

Este código define dos clases, `Persona` y `Empleado`, y crea objetos de ambas clases. La clase `Persona` tiene dos propiedades privadas, `nombre` y `edad`, y dos métodos públicos, `getNombre()` y `getEdad()`. La clase `Empleado` hereda de la clase `Persona` y tiene dos propiedades privadas adicionales, `salario` y `puesto`, y dos métodos públicos adicionales, `getSalario()` y `getPuesto()`.

El código crea un objeto `Persona` llamado `persona` y un objeto `Empleado` llamado `empleado`. Luego, imprime el nombre, la edad, el salario y el puesto del objeto `persona` y del objeto `empleado`.