```php
<?php

// Definición de las variables
$nombre = "Juan";
$apellido = "García";
$edad = 25;
$ciudad = "Madrid";
$país = "España";

// Creación de una estructura de datos para almacenar la información
$persona = [
    "nombre" => $nombre,
    "apellido" => $apellido,
    "edad" => $edad,
    "ciudad" => $ciudad,
    "país" => $país
];

// Función para mostrar la información de la persona
function mostrarPersona($persona) {
    echo "Nombre: " . $persona["nombre"] . "\n";
    echo "Apellido: " . $persona["apellido"] . "\n";
    echo "Edad: " . $persona["edad"] . "\n";
    echo "Ciudad: " . $persona["ciudad"] . "\n";
    echo "País: " . $persona["país"] . "\n";
}

// Llamada a la función para mostrar la información de la persona
mostrarPersona($persona);

// Creación de una estructura de datos para almacenar una lista de personas
$personas = [];

// Adición de la persona a la lista de personas
array_push($personas, $persona);

// Creación de una función para mostrar la lista de personas
function mostrarPersonas($personas) {
    foreach ($personas as $persona) {
        mostrarPersona($persona);
        echo "\n";
    }
}

// Llamada a la función para mostrar la lista de personas
mostrarPersonas($personas);

// Creación de una clase para representar a una persona
class Persona {
    private $nombre;
    private $apellido;
    private $edad;
    private $ciudad;
    private $país;

    public function __construct($nombre, $apellido, $edad, $ciudad, $país) {
        $this->nombre = $nombre;
        $this->apellido = $apellido;
        $this->edad = $edad;
        $this->ciudad = $ciudad;
        $this->país = $país;
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

    public function getCiudad() {
        return $this->ciudad;
    }

    public function getPaís() {
        return $this->país;
    }

    public function mostrar() {
        echo "Nombre: " . $this->nombre . "\n";
        echo "Apellido: " . $this->apellido . "\n";
        echo "Edad: " . $this->edad . "\n";
        echo "Ciudad: " . $this->ciudad . "\n";
        echo "País: " . $this->país . "\n";
    }
}

// Creación de una instancia de la clase Persona
$persona1 = new Persona("Juan", "García", 25, "Madrid", "España");

// Llamada al método mostrar de la clase Persona
$persona1->mostrar();

// Creación de una lista de instancias de la clase Persona
$personas2 = [];

// Adición de la instancia de la clase Persona a la lista
array_push($personas2, $persona1);

// Creación de una función para mostrar la lista de instancias de la clase Persona
function mostrarPersonas2($personas) {
    foreach ($personas as $persona) {
        $persona->mostrar();
        echo "\n";
    }
}

// Llamada a la función para mostrar la lista de instancias de la clase Persona
mostrarPersonas2($personas2);

```

Explicación del código:

* Se definen las variables `$nombre`, `$apellido`, `$edad`, `$ciudad` y `$país` con los valores de la información de una persona.
* Se crea una estructura de datos `$persona` para almacenar la información de la persona.
* Se define una función `mostrarPersona()` para mostrar la información de la persona.
* Se llama a la función `mostrarPersona()` para mostrar la información de la persona.
* Se crea una estructura de datos `$personas` para almacenar una lista de personas.
* Se añade la persona a la lista de personas.
* Se define una función `mostrarPersonas()` para mostrar la lista de personas.
* Se llama a la función `mostrarPersonas()` para mostrar la lista de personas.
* Se crea una clase `Persona` para representar a una persona.
* Se define un constructor en la clase `Persona` para inicializar las propiedades de la persona.
* Se definen métodos en la clase `Persona` para obtener las propiedades de la persona.
* Se define un método `mostrar()` en la clase `Persona` para mostrar la información de la persona.
* Se crea una instancia de la clase `Persona`.
* Se llama al método `mostrar()` de la clase `Persona` para mostrar la información de la persona.
* Se crea una lista de instancias de la clase `Persona`.
* Se añade la instancia de la clase `Persona` a la lista.
* Se define una función `mostrarPersonas2()` para mostrar la lista de instancias de la clase `Persona`.
* Se llama a la función `mostrarPersonas2()` para mostrar la lista de instancias de la clase `Persona`.