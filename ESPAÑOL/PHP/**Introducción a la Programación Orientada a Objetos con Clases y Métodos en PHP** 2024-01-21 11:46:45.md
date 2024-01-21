```php
<?php
// Definición de clases

// Clase Persona
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

    public function presentarse() {
        echo "Hola, mi nombre es $this->nombre $this->apellido y tengo $this->edad años.";
    }
}

// Clase Estudiante
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

    public function presentarse() {
        parent::presentarse();
        echo " Soy estudiante de la carrera $this->carrera con matrícula $this->matricula.";
    }
}

// Clase Profesor
class Profesor extends Persona {
    private $idProfesor;
    private $departamento;

    public function __construct($nombre, $apellido, $edad, $idProfesor, $departamento) {
        parent::__construct($nombre, $apellido, $edad);
        $this->idProfesor = $idProfesor;
        $this->departamento = $departamento;
    }

    public function getIdProfesor() {
        return $this->idProfesor;
    }

    public function getDepartamento() {
        return $this->departamento;
    }

    public function setIdProfesor($idProfesor) {
        $this->idProfesor = $idProfesor;
    }

    public function setDepartamento($departamento) {
        $this->departamento = $departamento;
    }

    public function presentarse() {
        parent::presentarse();
        echo " Soy profesor del departamento $this->departamento con ID $this->idProfesor.";
    }
}

// Creación de objetos

$persona1 = new Persona('Juan', 'Pérez', 25);
$estudiante1 = new Estudiante('María', 'Gómez', 20, '12345', 'Ingeniería en Sistemas');
$profesor1 = new Profesor('Pedro', 'Rodríguez', 40, '101010', 'Matemáticas');

// Uso de los objetos

$persona1->presentarse();
echo "<br>";
$estudiante1->presentarse();
echo "<br>";
$profesor1->presentarse();

```

Explicación del código:

* Se definen tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Estudiante` hereda de la clase `Persona`, y la clase `Profesor` hereda de la clase `Persona`.
* Las clases `Persona`, `Estudiante` y `Profesor` tienen atributos privados para almacenar el nombre, apellido, edad, matrícula y carrera del estudiante, y el ID del profesor y el departamento al que pertenece.
* Las clases `Persona`, `Estudiante` y `Profesor` tienen métodos públicos para obtener y modificar los atributos privados.
* La clase `Persona` tiene un método `presentarse()` que imprime el nombre, apellido y edad de la persona.
* La clase `Estudiante` tiene un método `presentarse()` que imprime el nombre, apellido, edad, matrícula y carrera del estudiante.
* La clase `Profesor` tiene un método `presentarse()` que imprime el nombre, apellido, edad, ID del profesor y departamento al que pertenece.
* Se crean tres objetos: `persona1`, `estudiante1` y `profesor1` de las clases `Persona`, `Estudiante` y `Profesor` respectivamente.
* Se utilizan los objetos `persona1`, `estudiante1` y `profesor1` para llamar al método `presentarse()` y mostrar la información de cada uno.