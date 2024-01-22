```php
<?php

// Definición de la clase Persona
class Persona {
    // Propiedades privadas
    private $nombre;
    private $apellido;
    private $edad;

    // Constructor
    public function __construct($nombre, $apellido, $edad) {
        $this->nombre = $nombre;
        $this->apellido = $apellido;
        $this->edad = $edad;
    }

    // Métodos públicos
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

    public function saludar() {
        echo "Hola, mi nombre es ".$this->nombre." ".$this->apellido." y tengo ".$this->edad." años.";
    }
}

// Definición de la clase Empleado
class Empleado extends Persona {
    // Propiedades privadas
    private $salario;
    private $puesto;

    // Constructor
    public function __construct($nombre, $apellido, $edad, $salario, $puesto) {
        parent::__construct($nombre, $apellido, $edad);
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

    public function trabajar() {
        echo "Estoy trabajando como ".$this->puesto." y gano ".$this->salario." euros al mes.";
    }
}

// Definición de la clase Cliente
class Cliente extends Persona {
    // Propiedades privadas
    private $email;
    private $telefono;

    // Constructor
    public function __construct($nombre, $apellido, $edad, $email, $telefono) {
        parent::__construct($nombre, $apellido, $edad);
        $this->email = $email;
        $this->telefono = $telefono;
    }

    // Métodos públicos
    public function getEmail() {
        return $this->email;
    }

    public function getTelefono() {
        return $this->telefono;
    }

    public function setEmail($email) {
        $this->email = $email;
    }

    public function setTelefono($telefono) {
        $this->telefono = $telefono;
    }

    public function comprar() {
        echo "Estoy comprando en la tienda.";
    }
}

// Crear objetos de las clases Persona, Empleado y Cliente
$persona1 = new Persona("Juan", "García", 25);
$empleado1 = new Empleado("María", "López", 30, 1500, "Ingeniero");
$cliente1 = new Cliente("Pedro", "Sánchez", 40, "pedro@ejemplo.com", "654123456");

// Llamar a los métodos de los objetos
$persona1->saludar();
$empleado1->trabajar();
$cliente1->comprar();

?>
```

Este código define tres clases: Persona, Empleado y Cliente. La clase Persona tiene las propiedades nombre, apellido y edad, y los métodos getNombre(), getApellido(), getEdad(), setNombre(), setApellido() y setEdad(). La clase Empleado hereda de la clase Persona y tiene las propiedades salario y puesto, y los métodos getSalario(), getPuesto(), setSalario(), setPuesto() y trabajar(). La clase Cliente también hereda de la clase Persona y tiene las propiedades email y telefono, y los métodos getEmail(), getTelefono(), setEmail(), setTelefono() y comprar().

El código crea objetos de las clases Persona, Empleado y Cliente y llama a los métodos de los objetos.