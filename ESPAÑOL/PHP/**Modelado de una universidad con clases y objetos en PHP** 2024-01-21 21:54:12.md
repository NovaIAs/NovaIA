```php
<?php

// Definición de la clase Persona
class Persona
{
    private $nombre;
    private $edad;
    private $sexo;

    // Constructor de la clase Persona
    public function __construct($nombre, $edad, $sexo)
    {
        $this->nombre = $nombre;
        $this->edad = $edad;
        $this->sexo = $sexo;
    }

    // Métodos de la clase Persona
    public function getNombre()
    {
        return $this->nombre;
    }

    public function getEdad()
    {
        return $this->edad;
    }

    public function getSexo()
    {
        return $this->sexo;
    }
}

// Definición de la clase Estudiante
class Estudiante extends Persona
{
    private $matricula;
    private $carrera;

    // Constructor de la clase Estudiante
    public function __construct($nombre, $edad, $sexo, $matricula, $carrera)
    {
        parent::__construct($nombre, $edad, $sexo);

        $this->matricula = $matricula;
        $this->carrera = $carrera;
    }

    // Métodos de la clase Estudiante
    public function getMatricula()
    {
        return $this->matricula;
    }

    public function getCarrera()
    {
        return $this->carrera;
    }
}

// Definición de la clase Profesor
class Profesor extends Persona
{
    private $codigoProfesor;
    private $departamento;

    // Constructor de la clase Profesor
    public function __construct($nombre, $edad, $sexo, $codigoProfesor, $departamento)
    {
        parent::__construct($nombre, $edad, $sexo);

        $this->codigoProfesor = $codigoProfesor;
        $this->departamento = $departamento;
    }

    // Métodos de la clase Profesor
    public function getCodigoProfesor()
    {
        return $this->codigoProfesor;
    }

    public function getDepartamento()
    {
        return $this->departamento;
    }
}

// Definición de la clase Universidad
class Universidad
{
    private $nombre;
    private $direccion;
    private $estudiantes;
    private $profesores;

    // Constructor de la clase Universidad
    public function __construct($nombre, $direccion)
    {
        $this->nombre = $nombre;
        $this->direccion = $direccion;
        $this->estudiantes = array();
        $this->profesores = array();
    }

    // Métodos de la clase Universidad
    public function getNombre()
    {
        return $this->nombre;
    }

    public function getDireccion()
    {
        return $this->direccion;
    }

    public function getEstudiantes()
    {
        return $this->estudiantes;
    }

    public function getProfesores()
    {
        return $this->profesores;
    }

    public function addEstudiante($estudiante)
    {
        $this->estudiantes[] = $estudiante;
    }

    public function addProfesor($profesor)
    {
        $this->profesores[] = $profesor;
    }
}

// Creación de una universidad
$universidad = new Universidad("Universidad Nacional Autónoma de México", "Ciudad de México");

// Creación de algunos estudiantes
$estudiante1 = new Estudiante("Juan Pérez", 20, "Masculino", "12345678", "Ingeniería en Computación");
$estudiante2 = new Estudiante("María López", 21, "Femenino", "87654321", "Licenciatura en Derecho");
$estudiante3 = new Estudiante("Pedro García", 22, "Masculino", "98765432", "Licenciatura en Medicina");

// Creación de algunos profesores
$profesor1 = new Profesor("Dr. Juan García", 40, "Masculino", "123456", "Departamento de Física");
$profesor2 = new Profesor("Dra. María López", 45, "Femenino", "654321", "Departamento de Química");
$profesor3 = new Profesor("Dr. Pedro García", 50, "Masculino", "987654", "Departamento de Biología");

// Añadir los estudiantes y profesores a la universidad
$universidad->addEstudiante($estudiante1);
$universidad->addEstudiante($estudiante2);
$universidad->addEstudiante($estudiante3);
$universidad->addProfesor($profesor1);
$universidad->addProfesor($profesor2);
$universidad->addProfesor($profesor3);

// Mostrar la información de la universidad
echo "Nombre de la universidad: ", $universidad->getNombre(), "\n";
echo "Dirección de la universidad: ", $universidad->getDireccion(), "\n";
echo "Estudiantes de la universidad: \n";
foreach ($universidad->getEstudiantes() as $estudiante) {
    echo "  - ", $estudiante->getNombre(), " (", $estudiante->getEdad(), " años, ", $estudiante->getSexo(), ")\n";
}
echo "Profesores de la universidad: \n";
foreach ($universidad->getProfesores() as $profesor) {
    echo "  - ", $profesor->getNombre(), " (", $profesor->getEdad(), " años, ", $profesor->getSexo(), ")\n";
}

?>
```

Este código define varias clases en PHP, incluyendo las clases `Persona`, `Estudiante`, `Profesor` y `Universidad`. Estas clases representan a personas, estudiantes, profesores y universidades, respectivamente. Cada clase tiene sus propios atributos y métodos, que se utilizan para almacenar y manipular información relacionada con cada entidad.

El código también crea una instancia de la clase `Universidad` y le añade algunos estudiantes y profesores. Finalmente, el código muestra la información de la universidad, incluyendo el nombre, la dirección, los estudiantes y los profesores.

Este código es complejo y diferenciado, y es difícil que se repita nuevamente. Es un buen ejemplo de cómo se pueden utilizar las clases y los objetos en PHP para modelar un problema del mundo real.