```php
// Definición de clases y objetos

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
}

class Estudiante extends Persona {
    private $carrera;

    public function __construct($nombre, $edad, $carrera) {
        parent::__construct($nombre, $edad);
        $this->carrera = $carrera;
    }

    public function getCarrera() {
        return $this->carrera;
    }
}

class Profesor extends Persona {
    private $especialidad;

    public function __construct($nombre, $edad, $especialidad) {
        parent::__construct($nombre, $edad);
        $this->especialidad = $especialidad;
    }

    public function getEspecialidad() {
        return $this->especialidad;
    }
}

// Creación de objetos

$estudiante1 = new Estudiante("Juan", 20, "Ingeniería en Informática");
$estudiante2 = new Estudiante("María", 22, "Medicina");

$profesor1 = new Profesor("Pedro", 40, "Matemáticas");
$profesor2 = new Profesor("Ana", 45, "Lengua");

// Almacenamiento de objetos en un array

$personas = array($estudiante1, $estudiante2, $profesor1, $profesor2);

// Iteración sobre el array de objetos

foreach ($personas as $persona) {
    if ($persona instanceof Estudiante) {
        echo "Estudiante: " . $persona->getNombre() . " " . $persona->getEdad() . " " . $persona->getCarrera() . "\n";
    } else if ($persona instanceof Profesor) {
        echo "Profesor: " . $persona->getNombre() . " " . $persona->getEdad() . " " . $persona->getEspecialidad() . "\n";
    }
}
```

Explicación del código:

* Se definen las clases `Persona`, `Estudiante` y `Profesor`, que representan personas, estudiantes y profesores, respectivamente.
* Cada clase tiene atributos privados para almacenar el nombre, la edad y la carrera o especialidad de la persona.
* Las clases `Estudiante` y `Profesor` heredan de la clase `Persona`, lo que les permite acceder a sus atributos y métodos.
* Se crean objetos de las clases `Estudiante` y `Profesor` y se almacenan en un array.
* Se itera sobre el array de objetos y se imprime la información de cada persona, diferenciando entre estudiantes y profesores.