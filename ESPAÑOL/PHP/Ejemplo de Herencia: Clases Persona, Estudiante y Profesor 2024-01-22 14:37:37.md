```php
<?php

// Definir una clase padre llamada "Persona"
class Persona {
    // Propiedades de la clase Persona
    protected $nombre;
    protected $apellido;
    protected $edad;

    // Constructor de la clase Persona
    public function __construct($nombre, $apellido, $edad) {
        $this->nombre = $nombre;
        $this->apellido = $apellido;
        $this->edad = $edad;
    }

    // Método de la clase Persona para obtener el nombre completo
    public function obtenerNombreCompleto() {
        return $this->nombre . " " . $this->apellido;
    }

    // Método de la clase Persona para obtener la edad
    public function obtenerEdad() {
        return $this->edad;
    }
}

// Definir una clase hija llamada "Estudiante" que hereda de la clase "Persona"
class Estudiante extends Persona {
    // Propiedades adicionales de la clase Estudiante
    protected $matricula;
    protected $carrera;

    // Constructor de la clase Estudiante
    public function __construct($nombre, $apellido, $edad, $matricula, $carrera) {
        parent::__construct($nombre, $apellido, $edad);
        $this->matricula = $matricula;
        $this->carrera = $carrera;
    }

    // Método de la clase Estudiante para obtener la matrícula
    public function obtenerMatricula() {
        return $this->matricula;
    }

    // Método de la clase Estudiante para obtener la carrera
    public function obtenerCarrera() {
        return $this->carrera;
    }
}

// Definir una clase hija llamada "Profesor" que hereda de la clase "Persona"
class Profesor extends Persona {
    // Propiedades adicionales de la clase Profesor
    protected $especialidad;
    protected $departamento;

    // Constructor de la clase Profesor
    public function __construct($nombre, $apellido, $edad, $especialidad, $departamento) {
        parent::__construct($nombre, $apellido, $edad);
        $this->especialidad = $especialidad;
        $this->departamento = $departamento;
    }

    // Método de la clase Profesor para obtener la especialidad
    public function obtenerEspecialidad() {
        return $this->especialidad;
    }

    // Método de la clase Profesor para obtener el departamento
    public function obtenerDepartamento() {
        return $this->departamento;
    }
}

// Crear un objeto de la clase Estudiante
$estudiante1 = new Estudiante("Juan", "García", 20, "123456", "Ingeniería en Informática");

// Crear un objeto de la clase Profesor
$profesor1 = new Profesor("María", "Pérez", 45, "Matemáticas", "Ciencias Exactas");

// Imprimir el nombre completo del estudiante
echo "Nombre del estudiante: " . $estudiante1->obtenerNombreCompleto() . "\n";

// Imprimir la matrícula del estudiante
echo "Matrícula del estudiante: " . $estudiante1->obtenerMatricula() . "\n";

// Imprimir la carrera del estudiante
echo "Carrera del estudiante: " . $estudiante1->obtenerCarrera() . "\n";

// Imprimir el nombre completo del profesor
echo "\nNombre del profesor: " . $profesor1->obtenerNombreCompleto() . "\n";

// Imprimir la especialidad del profesor
echo "Especialidad del profesor: " . $profesor1->obtenerEspecialidad() . "\n";

// Imprimir el departamento del profesor
echo "Departamento del profesor: " . $profesor1->obtenerDepartamento() . "\n";

```

Explicación del código:

1. Definimos una clase padre llamada "Persona" que tiene las propiedades "nombre", "apellido" y "edad", y los métodos "obtenerNombreCompleto()" y "obtenerEdad()".

2. Definimos una clase hija llamada "Estudiante" que hereda de la clase "Persona" y tiene las propiedades adicionales "matricula" y "carrera", y los métodos "obtenerMatricula()" y "obtenerCarrera()".

3. Definimos una clase hija llamada "Profesor" que hereda de la clase "Persona" y tiene las propiedades adicionales "especialidad" y "departamento", y los métodos "obtenerEspecialidad()" y "obtenerDepartamento()".

4. Creamos un objeto de la clase Estudiante llamado "$estudiante1" y le pasamos los valores para sus propiedades.

5. Creamos un objeto de la clase Profesor llamado "$profesor1" y le pasamos los valores para sus propiedades.

6. Imprimimos el nombre completo, la matrícula y la carrera del estudiante usando los métodos de la clase Estudiante.

7. Imprimimos el nombre completo, la especialidad y el departamento del profesor usando los métodos de la clase Profesor.

Este código es un ejemplo de cómo crear clases y objetos en PHP, y cómo las clases hijas pueden heredar propiedades y métodos de las clases padres.