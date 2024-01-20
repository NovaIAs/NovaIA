```php
<?php

// Definición de la clase Persona
class Persona {

    // Atributos de la clase
    private $nombre;
    private $edad;
    private $sexo;

    // Constructor de la clase
    public function __construct($nombre, $edad, $sexo) {
        $this->nombre = $nombre;
        $this->edad = $edad;
        $this->sexo = $sexo;
    }

    // Métodos de la clase
    public function saludar() {
        echo "Hola, mi nombre es $this->nombre, tengo $this->edad años y soy $this->sexo.\n";
    }

    public function despedirse() {
        echo "Adiós, fue un placer conocerte, $this->nombre.\n";
    }

}

// Definición de la clase Estudiante
class Estudiante extends Persona {

    // Atributos de la clase
    private $carrera;
    private $notaMedia;

    // Constructor de la clase
    public function __construct($nombre, $edad, $sexo, $carrera, $notaMedia) {
        parent::__construct($nombre, $edad, $sexo);
        $this->carrera = $carrera;
        $this->notaMedia = $notaMedia;
    }

    // Métodos de la clase
    public function estudiar() {
        echo "$this->nombre está estudiando para el examen de $this->carrera.\n";
    }

    public function aprobar() {
        if ($this->notaMedia >= 5) {
            echo "$this->nombre ha aprobado el examen de $this->carrera.\n";
        } else {
            echo "$this->nombre ha suspendido el examen de $this->carrera.\n";
        }
    }

}

// Definición de la clase Profesor
class Profesor extends Persona {

    // Atributos de la clase
    private $asignatura;
    private $sueldo;

    // Constructor de la clase
    public function __construct($nombre, $edad, $sexo, $asignatura, $sueldo) {
        parent::__construct($nombre, $edad, $sexo);
        $this->asignatura = $asignatura;
        $this->sueldo = $sueldo;
    }

    // Métodos de la clase
    public function darClase() {
        echo "$this->nombre está dando clase de $this->asignatura.\n";
    }

    public function cobrarSueldo() {
        echo "$this->nombre ha cobrado su sueldo de $this->sueldo euros.\n";
    }

}

// Creación de objetos de las clases Persona, Estudiante y Profesor
$persona1 = new Persona("Juan", 20, "hombre");
$persona2 = new Estudiante("María", 22, "mujer", "Ingeniería Informática", 7.5);
$persona3 = new Profesor("Luis", 40, "hombre", "Matemáticas", 2000);

// Llamada a los métodos de los objetos
$persona1->saludar();
$persona2->estudiar();
$persona2->aprobar();
$persona3->darClase();
$persona3->cobrarSueldo();

```

Explicación del código:

* La clase `Persona` define los atributos y métodos comunes a todas las personas.
* La clase `Estudiante` hereda de la clase `Persona` y define los atributos y métodos específicos de los estudiantes.
* La clase `Profesor` hereda de la clase `Persona` y define los atributos y métodos específicos de los profesores.
* Se crean tres objetos de las clases `Persona`, `Estudiante` y `Profesor`.
* Se llaman a los métodos de los objetos para mostrar sus atributos y realizar sus acciones.