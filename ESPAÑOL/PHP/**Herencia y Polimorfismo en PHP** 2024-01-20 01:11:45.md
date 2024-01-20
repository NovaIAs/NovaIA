```php
<?php

// Definición de la clase Persona
class Persona {
    // Atributos privados
    private $nombre;
    private $edad;

    // Constructor
    public function __construct($nombre, $edad) {
        $this->nombre = $nombre;
        $this->edad = $edad;
    }

    // Métodos públicos
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

// Definición de la clase Empleado
class Empleado extends Persona {
    // Atributos privados
    private $salario;
    private $puesto;

    // Constructor
    public function __construct($nombre, $edad, $salario, $puesto) {
        // Llamar al constructor de la clase padre
        parent::__construct($nombre, $edad);

        $this->salario = $salario;
        $this->puesto = $puesto;
    }

    // Métodos públicos
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

// Definición de la clase Empresa
class Empresa {
    // Atributos privados
    private $nombre;
    private $empleados;

    // Constructor
    public function __construct($nombre) {
        $this->nombre = $nombre;
        $this->empleados = array();
    }

    // Métodos públicos
    public function getNombre() {
        return $this->nombre;
    }

    public function getEmpleados() {
        return $this->empleados;
    }

    public function addEmpleado(Empleado $empleado) {
        $this->empleados[] = $empleado;
    }

    public function removeEmpleado(Empleado $empleado) {
        $key = array_search($empleado, $this->empleados);

        if ($key !== false) {
            unset($this->empleados[$key]);
        }
    }
}

// Crear una nueva empresa
$empresa = new Empresa("Acme Industries");

// Crear nuevos empleados
$empleado1 = new Empleado("Juan Pérez", 30, 1000, "Programador");
$empleado2 = new Empleado("Ana García", 25, 1200, "Diseñador");
$empleado3 = new Empleado("Pedro López", 40, 1500, "Gerente");

// Añadir los empleados a la empresa
$empresa->addEmpleado($empleado1);
$empresa->addEmpleado($empleado2);
$empresa->addEmpleado($empleado3);

// Obtener los empleados de la empresa
$empleados = $empresa->getEmpleados();

// Recorrer los empleados y mostrar su información
foreach ($empleados as $empleado) {
    echo "Nombre: " . $empleado->getNombre() . "<br>";
    echo "Edad: " . $empleado->getEdad() . "<br>";
    echo "Salario: " . $empleado->getSalario() . "<br>";
    echo "Puesto: " . $empleado->getPuesto() . "<br><br>";
}

?>
```

Este código crea una clase Persona con dos atributos privados: nombre y edad. También define una clase Empleado que hereda de la clase Persona y añade dos atributos privados más: salario y puesto.

A continuación, se crea una clase Empresa que tiene un atributo privado llamado empleados, que es una matriz de objetos Empleado.

El código crea una nueva empresa llamada "Acme Industries" y tres nuevos empleados. A continuación, añade los empleados a la empresa y obtiene una lista de los empleados de la empresa.

Por último, el código recorre la lista de empleados y muestra su información en la salida.