```php
<?php

// Definición de la clase Persona
class Persona {

    // Propiedades de la clase
    private $nombre;
    private $apellido;
    private $edad;

    // Constructor de la clase
    public function __construct($nombre, $apellido, $edad) {
        $this->nombre = $nombre;
        $this->apellido = $apellido;
        $this->edad = $edad;
    }

    // Métodos de la clase
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

}

// Definición de la clase Estudiante
class Estudiante extends Persona {

    // Propiedades de la clase
    private $matricula;
    private $carrera;

    // Constructor de la clase
    public function __construct($nombre, $apellido, $edad, $matricula, $carrera) {
        parent::__construct($nombre, $apellido, $edad);
        $this->matricula = $matricula;
        $this->carrera = $carrera;
    }

    // Métodos de la clase
    public function getMatricula() {
        return $this->matricula;
    }

    public function getCarrera() {
        return $this->carrera;
    }

    public function setMatricula($matricula) {
        $this->matricula = $matricula;
    }

    public function setCarrera($carrera) {
        $this->carrera = $carrera;
    }

}

// Definición de la clase Profesor
class Profesor extends Persona {

    // Propiedades de la clase
    private $departamento;
    private $salario;

    // Constructor de la clase
    public function __construct($nombre, $apellido, $edad, $departamento, $salario) {
        parent::__construct($nombre, $apellido, $edad);
        $this->departamento = $departamento;
        $this->salario = $salario;
    }

    // Métodos de la clase
    public function getDepartamento() {
        return $this->departamento;
    }

    public function getSalario() {
        return $this->salario;
    }

    public function setDepartamento($departamento) {
        $this->departamento = $departamento;
    }

    public function setSalario($salario) {
        $this->salario = $salario;
    }

}

// Definición de la clase Administrativo
class Administrativo extends Persona {

    // Propiedades de la clase
    private $puesto;
    private $sueldo;

    // Constructor de la clase
    public function __construct($nombre, $apellido, $edad, $puesto, $sueldo) {
        parent::__construct($nombre, $apellido, $edad);
        $this->puesto = $puesto;
        $this->sueldo = $sueldo;
    }

    // Métodos de la clase
    public function getPuesto() {
        return $this->puesto;
    }

    public function getSueldo() {
        return $this->sueldo;
    }

    public function setPuesto($puesto) {
        $this->puesto = $puesto;
    }

    public function setSueldo($sueldo) {
        $this->sueldo = $sueldo;
    }

}

// Crear una instancia de la clase Estudiante
$estudiante1 = new Estudiante("Juan", "García", 20, "123456789", "Ingeniería Informática");

// Crear una instancia de la clase Profesor
$profesor1 = new Profesor("María", "López", 45, "Departamento de Matemáticas", 2500);

// Crear una instancia de la clase Administrativo
$administrativo1 = new Administrativo("Pedro", "Sánchez", 30, "Secretario", 1800);

// Imprimir los datos de la persona
echo "Nombre: " . $estudiante1->getNombre() . "\n";
echo "Apellido: " . $estudiante1->getApellido() . "\n";
echo "Edad: " . $estudiante1->getEdad() . "\n";
echo "Matricula: " . $estudiante1->getMatricula() . "\n";
echo "Carrera: " . $estudiante1->getCarrera() . "\n";

echo "\n";

// Imprimir los datos de la persona
echo "Nombre: " . $profesor1->getNombre() . "\n";
echo "Apellido: " . $profesor1->getApellido() . "\n";
echo "Edad: " . $profesor1->getEdad() . "\n";
echo "Departamento: " . $profesor1->getDepartamento() . "\n";
echo "Salario: " . $profesor1->getSalario() . "\n";

echo "\n";

// Imprimir los datos de la persona
echo "Nombre: " . $administrativo1->getNombre() . "\n";
echo "Apellido: " . $administrativo1->getApellido() . "\n";
echo "Edad: " . $administrativo1->getEdad() . "\n";
echo "Puesto: " . $administrativo1->getPuesto() . "\n";
echo "Sueldo: " . $administrativo1->getSueldo() . "\n";

?>
```

El código anterior define tres clases: Persona, Estudiante, Profesor y Administrativo. La clase Persona es la clase padre de las otras tres clases. La clase Estudiante hereda de la clase Persona y añade las propiedades matrícula y carrera. La clase Profesor hereda de la clase Persona y añade las propiedades departamento y salario. La clase Administrativo hereda de la clase Persona y añade las propiedades puesto y sueldo.

El código también crea una instancia de cada una de las clases y luego imprime los datos de cada persona.