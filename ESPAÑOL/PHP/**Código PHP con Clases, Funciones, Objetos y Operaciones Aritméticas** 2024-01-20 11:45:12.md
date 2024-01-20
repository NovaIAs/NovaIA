```php
<?php

// Definición de clases y funciones

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

    public function saludar() {
        echo "Hola, mi nombre es $this->nombre y tengo $this->edad años.";
    }
}

function sumar($a, $b) {
    return $a + $b;
}

function restar($a, $b) {
    return $a - $b;
}

function multiplicar($a, $b) {
    return $a * $b;
}

function dividir($a, $b) {
    return $a / $b;
}

// Creación de objetos y variables

$persona1 = new Persona("Juan", 20);
$persona2 = new Persona("María", 25);

$suma = sumar(10, 5);
$resta = restar(10, 5);
$multiplicacion = multiplicar(10, 5);
$division = dividir(10, 5);

// Impresión de resultados

echo "La suma de 10 y 5 es $suma.";
echo "La resta de 10 y 5 es $resta.";
echo "La multiplicación de 10 y 5 es $multiplicacion.";
echo "La división de 10 y 5 es $division.";

$persona1->saludar();
$persona2->saludar();

?>
```

Explicación del código:

* Se definen las clases `Persona` y las funciones `sumar`, `restar`, `multiplicar` y `dividir`.
* Se crean los objetos `persona1` y `persona2` de la clase `Persona`.
* Se crean las variables `suma`, `resta`, `multiplicacion` y `division` con los resultados de las operaciones aritméticas.
* Se imprimen los resultados de las operaciones aritméticas y los saludos de las personas.