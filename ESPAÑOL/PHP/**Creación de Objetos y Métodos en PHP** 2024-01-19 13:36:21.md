```php
<?php

// Definimos una clase llamada "Persona"
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

    public function saludar() {
        echo "Hola, mi nombre es {$this->nombre} {$this->apellido} y tengo {$this->edad} años.";
    }
}

// Creamos un objeto de la clase "Persona"
$persona1 = new Persona("Juan", "García", 25);

// Accedemos a las propiedades del objeto
echo "Nombre: {$persona1->getNombre()}\n";
echo "Apellido: {$persona1->getApellido()}\n";
echo "Edad: {$persona1->getEdad()}\n";

// Modificamos las propiedades del objeto
$persona1->setNombre("María");
$persona1->setApellido("Pérez");
$persona1->setEdad(30);

// Accedemos de nuevo a las propiedades del objeto
echo "Nombre: {$persona1->getNombre()}\n";
echo "Apellido: {$persona1->getApellido()}\n";
echo "Edad: {$persona1->getEdad()}\n";

// Llamamos al método "saludar()" del objeto
$persona1->saludar();

?>
```

Explicación del código:

* Definimos una clase llamada "Persona" que tiene tres propiedades privadas: "nombre", "apellido" y "edad".
* El constructor de la clase inicializa las tres propiedades con los valores pasados como argumentos.
* La clase tiene varios métodos, incluyendo getters y setters para acceder y modificar las propiedades, y un método llamado "saludar()" que imprime un mensaje con el nombre, apellido y edad de la persona.
* Creamos un objeto de la clase "Persona" con el nombre "Juan", el apellido "García" y la edad "25".
* Accedemos a las propiedades del objeto y las imprimimos por pantalla.
* Modificamos las propiedades del objeto y las volvemos a imprimir por pantalla.
* Llamamos al método "saludar()" del objeto para imprimir un mensaje con el nombre, apellido y edad de la persona.