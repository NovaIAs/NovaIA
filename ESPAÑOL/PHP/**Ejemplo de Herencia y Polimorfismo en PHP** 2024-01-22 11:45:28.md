```php
<?php

// Definición de clases

class Persona {
    private $nombre;
    private $apellido;
    private $edad;

    public function __construct($nombre, $apellido, $edad) {
        $this->nombre = $nombre;
        $this->apellido = $apellido;
        $this->edad = $edad;
    }

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

class Estudiante extends Persona {
    private $matricula;
    private $carrera;

    public function __construct($nombre, $apellido, $edad, $matricula, $carrera) {
        parent::__construct($nombre, $apellido, $edad);
        $this->matricula = $matricula;
        $this->carrera = $carrera;
    }

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

class Profesor extends Persona {
    private $departamento;
    private $sueldo;

    public function __construct($nombre, $apellido, $edad, $departamento, $sueldo) {
        parent::__construct($nombre, $apellido, $edad);
        $this->departamento = $departamento;
        $this->sueldo = $sueldo;
    }

    public function getDepartamento() {
        return $this->departamento;
    }

    public function getSueldo() {
        return $this->sueldo;
    }

    public function setDepartamento($departamento) {
        $this->departamento = $departamento;
    }

    public function setSueldo($sueldo) {
        $this->sueldo = $sueldo;
    }
}

// Creación de objetos

$estudiante1 = new Estudiante('Juan', 'García', 20, '123456', 'Ingeniería Informática');
$estudiante2 = new Estudiante('María', 'Pérez', 21, '654321', 'Administración de Empresas');
$profesor1 = new Profesor('Pedro', 'López', 40, 'Departamento de Informática', 3000);
$profesor2 = new Profesor('Ana', 'Fernández', 45, 'Departamento de Economía', 3500);

// Almacenamiento de objetos en un array

$personas = array($estudiante1, $estudiante2, $profesor1, $profesor2);

// Recorrido del array e impresión de los datos de cada objeto

foreach ($personas as $persona) {
    if ($persona instanceof Estudiante) {
        echo 'Estudiante: ' . $persona->getNombre() . ' ' . $persona->getApellido() . ', ' . $persona->getEdad() . ' años, matrícula ' . $persona->getMatricula() . ', carrera ' . $persona->getCarrera() . '<br>';
    } elseif ($persona instanceof Profesor) {
        echo 'Profesor: ' . $persona->getNombre() . ' ' . $persona->getApellido() . ', ' . $persona->getEdad() . ' años, departamento ' . $persona->getDepartamento() . ', sueldo ' . $persona->getSueldo() . '<br>';
    }
}

?>
```

Explicación del código:

* Se definen las clases `Persona`, `Estudiante` y `Profesor`, que heredan de la clase `Persona`.
* Se crean objetos de las clases `Estudiante` y `Profesor`.
* Se almacenan los objetos en un array.
* Se recorre el array e se imprimen los datos de cada objeto.

Este código es complejo porque incluye múltiples clases, herencia, polimorfismo y un bucle `foreach`. También es difícil de repetir porque los nombres de las clases, los atributos y los métodos son únicos.