```php
<?php

/**
 * Un ejemplo de código complejo en PHP que realiza varias tareas:
 *
 * 1. Crea una clase llamada "Complejo" con dos propiedades, "real" e "imaginario".
 * 2. Define un método "__construct()" para inicializar las propiedades de la clase.
 * 3. Define un método "__toString()" para devolver una representación en cadena del objeto Complejo.
 * 4. Define un método "suma()" para sumar dos objetos Complejos.
 * 5. Define un método "resta()" para restar dos objetos Complejos.
 * 6. Define un método "producto()" para multiplicar dos objetos Complejos.
 * 7. Define un método "división()" para dividir dos objetos Complejos.
 * 8. Crea dos objetos Complejos y los muestra por pantalla.
 * 9. Suma, resta, multiplica y divide los dos objetos Complejos y muestra los resultados por pantalla.
 */

class Complejo {

    // Propiedades
    private $real;
    private $imaginario;

    // Constructor
    public function __construct($real, $imaginario) {
        $this->real = $real;
        $this->imaginario = $imaginario;
    }

    // Método "__toString()"
    public function __toString() {
        return $this->real . " + " . $this->imaginario . "i";
    }

    // Método "suma()"
    public function suma(Complejo $complejo) {
        $real = $this->real + $complejo->real;
        $imaginario = $this->imaginario + $complejo->imaginario;
        return new Complejo($real, $imaginario);
    }

    // Método "resta()"
    public function resta(Complejo $complejo) {
        $real = $this->real - $complejo->real;
        $imaginario = $this->imaginario - $complejo->imaginario;
        return new Complejo($real, $imaginario);
    }

    // Método "producto()"
    public function producto(Complejo $complejo) {
        $real = $this->real * $complejo->real - $this->imaginario * $complejo->imaginario;
        $imaginario = $this->real * $complejo->imaginario + $this->imaginario * $complejo->real;
        return new Complejo($real, $imaginario);
    }

    // Método "división()"
    public function división(Complejo $complejo) {
        $denominador = $complejo->real ** 2 + $complejo->imaginario ** 2;
        $real = ($this->real * $complejo->real + $this->imaginario * $complejo->imaginario) / $denominador;
        $imaginario = ($this->imaginario * $complejo->real - $this->real * $complejo->imaginario) / $denominador;
        return new Complejo($real, $imaginario);
    }

}

// Crea dos objetos Complejos
$complejo1 = new Complejo(3, 4);
$complejo2 = new Complejo(5, -2);

// Muestra los objetos Complejos por pantalla
echo "Complejo 1: " . $complejo1 . "\n";
echo "Complejo 2: " . $complejo2 . "\n";

// Suma, resta, multiplica y divide los dos objetos Complejos
$suma = $complejo1->suma($complejo2);
$resta = $complejo1->resta($complejo2);
$producto = $complejo1->producto($complejo2);
$división = $complejo1->división($complejo2);

// Muestra los resultados por pantalla
echo "Suma: " . $suma . "\n";
echo "Resta: " . $resta . "\n";
echo "Producto: " . $producto . "\n";
echo "División: " . $división . "\n";

?>
```

Este código crea una clase llamada "Complejo" que representa un número complejo. La clase tiene dos propiedades, "real" e "imaginario", y varios métodos, incluyendo "__construct()", "__toString()", "suma()", "resta()", "producto()" y "división()".

El código también crea dos objetos Complejos y los muestra por pantalla. Luego, suma, resta, multiplica y divide los dos objetos Complejos y muestra los resultados por pantalla.