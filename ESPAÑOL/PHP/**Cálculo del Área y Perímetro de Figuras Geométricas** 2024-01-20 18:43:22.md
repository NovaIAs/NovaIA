```php
<?php

// Definir una clase abstracta para representar una figura geométrica.
abstract class FiguraGeometrica {

    // Definir una propiedad privada para almacenar el nombre de la figura.
    private $nombre;

    // Definir un constructor para inicializar la propiedad `nombre`.
    public function __construct($nombre) {
        $this->nombre = $nombre;
    }

    // Definir un método abstracto para calcular el área de la figura.
    abstract public function calcularArea();

    // Definir un método abstracto para calcular el perímetro de la figura.
    abstract public function calcularPerimetro();

    // Definir un método para obtener el nombre de la figura.
    public function getNombre() {
        return $this->nombre;
    }
}

// Definir una clase que represente un cuadrado.
class Cuadrado extends FiguraGeometrica {

    // Definir una propiedad privada para almacenar la longitud del lado del cuadrado.
    private $lado;

    // Definir un constructor para inicializar la propiedad `lado`.
    public function __construct($lado) {
        parent::__construct('Cuadrado');
        $this->lado = $lado;
    }

    // Implementar el método abstracto para calcular el área del cuadrado.
    public function calcularArea() {
        return $this->lado * $this->lado;
    }

    // Implementar el método abstracto para calcular el perímetro del cuadrado.
    public function calcularPerimetro() {
        return 4 * $this->lado;
    }
}

// Definir una clase que represente un círculo.
class Circulo extends FiguraGeometrica {

    // Definir una propiedad privada para almacenar el radio del círculo.
    private $radio;

    // Definir un constructor para inicializar la propiedad `radio`.
    public function __construct($radio) {
        parent::__construct('Círculo');
        $this->radio = $radio;
    }

    // Implementar el método abstracto para calcular el área del círculo.
    public function calcularArea() {
        return pi() * $this->radio * $this->radio;
    }

    // Implementar el método abstracto para calcular el perímetro del círculo.
    public function calcularPerimetro() {
        return 2 * pi() * $this->radio;
    }
}

// Definir una clase que represente un triángulo.
class Triángulo extends FiguraGeometrica {

    // Definir propiedades privadas para almacenar las longitudes de los lados del triángulo.
    private $ladoA;
    private $ladoB;
    private $ladoC;

    // Definir un constructor para inicializar las propiedades `ladoA`, `ladoB` y `ladoC`.
    public function __construct($ladoA, $ladoB, $ladoC) {
        parent::__construct('Triángulo');
        $this->ladoA = $ladoA;
        $this->ladoB = $ladoB;
        $this->ladoC = $ladoC;
    }

    // Implementar el método abstracto para calcular el área del triángulo.
    public function calcularArea() {
        // Calcular el semiperímetro del triángulo.
        $semiperímetro = ($this->ladoA + $this->ladoB + $this->ladoC) / 2;

        // Calcular el área del triángulo usando la fórmula de Herón.
        return sqrt($semiperímetro * ($semiperímetro - $this->ladoA) * ($semiperímetro - $this->ladoB) * ($semiperímetro - $this->ladoC));
    }

    // Implementar el método abstracto para calcular el perímetro del triángulo.
    public function calcularPerimetro() {
        return $this->ladoA + $this->ladoB + $this->ladoC;
    }
}

// Crear un objeto de la clase `Cuadrado`.
$cuadrado = new Cuadrado(5);

// Crear un objeto de la clase `Círculo`.
$círculo = new Círculo(10);

// Crear un objeto de la clase `Triángulo`.
$triángulo = new Triángulo(3, 4, 5);

// Calcular y mostrar el área y el perímetro de cada figura.
echo "Cuadrado:\n";
echo "Área: " . $cuadrado->calcularArea() . "\n";
echo "Perímetro: " . $cuadrado->calcularPerimetro() . "\n\n";

echo "Círculo:\n";
echo "Área: " . $círculo->calcularArea() . "\n";
echo "Perímetro: " . $círculo->calcularPerimetro() . "\n\n";

echo "Triángulo:\n";
echo "Área: " . $triángulo->calcularArea() . "\n";
echo "Perímetro: " . $triángulo->calcularPerimetro() . "\n";
```

Explicación del código:

1. Definimos una clase abstracta `FiguraGeometrica` que representa una figura geométrica genérica. Esta clase contiene propiedades y métodos abstractos para calcular el área y el perímetro de la figura.

2. Definimos tres clases concretas que heredan de la clase `FiguraGeometrica`: `Cuadrado`, `Círculo` y `Triángulo`. Cada clase concreta implementa los métodos abstractos para calcular el área y el perímetro de la figura específica.

3. Creamos objetos de cada una de las clases concretas: `cuadrado`, `círculo` y `triángulo`.

4. Calculamos y mostramos el área y el perímetro de cada figura usando los métodos definidos en las clases concretas.