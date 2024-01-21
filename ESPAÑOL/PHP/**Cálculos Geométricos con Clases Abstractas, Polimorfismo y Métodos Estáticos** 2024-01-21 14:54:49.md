```php
// Definir una clase abstracta "Forma" que representa una forma geométrica
abstract class Forma {
    protected $ancho;
    protected $alto;

    // Definir un método abstracto "calcularArea()" para calcular el área de la forma
    abstract public function calcularArea(): float;
}

// Definir subclases de "Forma" incluyendo "Rectángulo" y "Triángulo"
class Rectangulo extends Forma {
    // Definir un constructor que reciba el ancho y el alto del rectángulo
    public function __construct($ancho, $alto) {
        $this->ancho = $ancho;
        $this->alto = $alto;
    }

    // Implementar el método abstracto "calcularArea()" para calcular el área del rectángulo
    public function calcularArea(): float {
        return $this->ancho * $this->alto;
    }
}

class Triangulo extends Forma {
    // Definir un constructor que reciba la base y la altura del triángulo
    public function __construct($base, $altura) {
        $this->base = $base;
        $this->altura = $altura;
    }

    // Implementar el método abstracto "calcularArea()" para calcular el área del triángulo
    public function calcularArea(): float {
        return (1 / 2) * $this->base * $this->altura;
    }
}

// Definir una clase "CalculadoraGeometrica" que contiene métodos estáticos para calcular el área y el perímetro de las formas
class CalculadoraGeometrica {
    public static function calcularAreaTotal(Forma $forma): float {
        return $forma->calcularArea();
    }

    public static function calcularPerimetroTotal(Forma $forma): float {
        // Utilizar polimorfismo para calcular el perímetro en función del tipo de forma
        if ($forma instanceof Rectangulo) {
            return 2 * ($forma->ancho + $forma->alto);
        } elseif ($forma instanceof Triangulo) {
            // Calcular el perímetro del triángulo
            $lado1 = sqrt(pow($forma->base / 2, 2) + pow($forma->altura, 2));
            $lado2 = $lado1;
            $lado3 = $forma->base;
            return $lado1 + $lado2 + $lado3;
        }
    }
}

// Crear objetos "Rectángulo" y "Triángulo"
$rectangulo1 = new Rectangulo(5, 10);
$triangulo1 = new Triangulo(6, 8);

// Calcular el área y el perímetro de cada forma
$areaRectangulo1 = CalculadoraGeometrica::calcularAreaTotal($rectangulo1);
$perimetroRectangulo1 = CalculadoraGeometrica::calcularPerimetroTotal($rectangulo1);
$areaTriangulo1 = CalculadoraGeometrica::calcularAreaTotal($triangulo1);
$perimetroTriangulo1 = CalculadoraGeometrica::calcularPerimetroTotal($triangulo1);

// Mostrar los resultados
echo "Área del Rectángulo 1: $areaRectangulo1 unidades cuadradas\n";
echo "Perímetro del Rectángulo 1: $perimetroRectangulo1 unidades\n";
echo "Área del Triángulo 1: $areaTriangulo1 unidades cuadradas\n";
echo "Perímetro del Triángulo 1: $perimetroTriangulo1 unidades\n";
```

Explicación del código:

* Se define una clase abstracta "Forma" que representa una forma geométrica, incluyendo un método abstracto "calcularArea()" que las subclases deben implementar.
* Se definen subclases "Rectángulo" y "Triángulo" que extienden de "Forma" e implementan el método "calcularArea()".
* Se define una clase "CalculadoraGeometrica" que contiene métodos estáticos para calcular el área y el perímetro de las formas.
* Se crean objetos "Rectángulo" y "Triángulo" y se calculan el área y el perímetro de cada forma utilizando los métodos de "CalculadoraGeometrica".
* Se muestran los resultados en la consola.

Este código es complejo porque involucra herencia, polimorfismo y métodos estáticos, y también maneja múltiples tipos de formas y cálculos. Es una demostración de cómo usar estos conceptos en PHP para construir un código más flexible y reutilizable.