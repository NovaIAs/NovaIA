```php
<?php

// Definición de la clase Persona
class Persona {

    // Propiedades de la clase Persona
    private $nombre;
    private $edad;
    private $sexo;

    // Constructor de la clase Persona
    public function __construct($nombre, $edad, $sexo) {
        $this->nombre = $nombre;
        $this->edad = $edad;
        $this->sexo = $sexo;
    }

    // Métodos de la clase Persona
    public function getNombre() {
        return $this->nombre;
    }

    public function getEdad() {
        return $this->edad;
    }

    public function getSexo() {
        return $this->sexo;
    }

    public function setNombre($nombre) {
        $this->nombre = $nombre;
    }

    public function setEdad($edad) {
        $this->edad = $edad;
    }

    public function setSexo($sexo) {
        $this->sexo = $sexo;
    }

    public function __toString() {
        return "Nombre: $this->nombre, Edad: $this->edad, Sexo: $this->sexo";
    }
}

// Definición de la clase Trabajador
class Trabajador extends Persona {

    // Propiedades de la clase Trabajador
    private $profesion;
    private $salario;

    // Constructor de la clase Trabajador
    public function __construct($nombre, $edad, $sexo, $profesion, $salario) {
        parent::__construct($nombre, $edad, $sexo);
        $this->profesion = $profesion;
        $this->salario = $salario;
    }

    // Métodos de la clase Trabajador
    public function getProfesion() {
        return $this->profesion;
    }

    public function getSalario() {
        return $this->salario;
    }

    public function setProfesion($profesion) {
        $this->profesion = $profesion;
    }

    public function setSalario($salario) {
        $this->salario = $salario;
    }

    public function __toString() {
        return parent::__toString() . ", Profesión: $this->profesion, Salario: $this->salario";
    }
}

// Definición de la clase Cliente
class Cliente extends Persona {

    // Propiedades de la clase Cliente
    private $direccion;
    private $telefono;

    // Constructor de la clase Cliente
    public function __construct($nombre, $edad, $sexo, $direccion, $telefono) {
        parent::__construct($nombre, $edad, $sexo);
        $this->direccion = $direccion;
        $this->telefono = $telefono;
    }

    // Métodos de la clase Cliente
    public function getDireccion() {
        return $this->direccion;
    }

    public function getTelefono() {
        return $this->telefono;
    }

    public function setDireccion($direccion) {
        $this->direccion = $direccion;
    }

    public function setTelefono($telefono) {
        $this->telefono = $telefono;
    }

    public function __toString() {
        return parent::__toString() . ", Dirección: $this->direccion, Teléfono: $this->telefono";
    }
}

// Creación de objetos de las clases Persona, Trabajador y Cliente
$persona1 = new Persona("Juan", 25, "Masculino");
$trabajador1 = new Trabajador("Pedro", 30, "Masculino", "Ingeniero", 2000);
$cliente1 = new Cliente("María", 35, "Femenino", "Calle Mayor, 123", "912345678");

// Impresión de los objetos creados
echo "<pre>";
echo "Persona 1: $persona1";
echo "\n";
echo "Trabajador 1: $trabajador1";
echo "\n";
echo "Cliente 1: $cliente1";
echo "</pre>";

?>
```

Explicación del código:

* Se definen tres clases: `Persona`, `Trabajador` y `Cliente`. La clase `Persona` es la clase base de las otras dos clases.
* Cada clase tiene sus propias propiedades y métodos. Las propiedades son los datos que pertenecen a una instancia de una clase, y los métodos son las acciones que una instancia de una clase puede realizar.
* La clase `Trabajador` hereda de la clase `Persona`, lo que significa que tiene todas las propiedades y métodos de la clase `Persona`, además de sus propias propiedades y métodos.
* La clase `Cliente` también hereda de la clase `Persona`, lo que significa que tiene todas las propiedades y métodos de la clase `Persona`, además de sus propias propiedades y métodos.
* Se crean objetos de las tres clases y se imprimen sus propiedades.