```php
<?php
// Definición de la clase abstracta Figura
abstract class Figura {
    // Propiedades protegidas para guardar la posición y el color de la figura
    protected $x;
    protected $y;
    protected $color;

    // Constructor para inicializar la posición y el color de la figura
    public function __construct($x, $y, $color) {
        $this->x = $x;
        $this->y = $y;
        $this->color = $color;
    }

    // Método abstracto para calcular el área de la figura
    abstract public function calcularArea();

    // Método abstracto para dibujar la figura
    abstract public function dibujar();
}

// Clase Círculo que hereda de la clase Figura
class Circulo extends Figura {
    // Propiedad protegida para guardar el radio del círculo
    protected $radio;

    // Constructor para inicializar el radio del círculo
    public function __construct($x, $y, $color, $radio) {
        parent::__construct($x, $y, $color);
        $this->radio = $radio;
    }

    // Método para calcular el área del círculo
    public function calcularArea() {
        return pi() * $this->radio ** 2;
    }

    // Método para dibujar el círculo
    public function dibujar() {
        echo "<circle cx=\"{$this->x}\" cy=\"{$this->y}\" r=\"{$this->radio}\" fill=\"{$this->color}\" />";
    }
}

// Clase Cuadrado que hereda de la clase Figura
class Cuadrado extends Figura {
    // Propiedad protegida para guardar el lado del cuadrado
    protected $lado;

    // Constructor para inicializar el lado del cuadrado
    public function __construct($x, $y, $color, $lado) {
        parent::__construct($x, $y, $color);
        $this->lado = $lado;
    }

    // Método para calcular el área del cuadrado
    public function calcularArea() {
        return $this->lado ** 2;
    }

    // Método para dibujar el cuadrado
    public function dibujar() {
        echo "<rect x=\"{$this->x}\" y=\"{$this->y}\" width=\"{$this->lado}\" height=\"{$this->lado}\" fill=\"{$this->color}\" />";
    }
}

// Clase Triángulo que hereda de la clase Figura
class Triángulo extends Figura {
    // Propiedades protegidas para guardar los puntos del triángulo
    protected $puntoA;
    protected $puntoB;
    protected $puntoC;

    // Constructor para inicializar los puntos del triángulo
    public function __construct($x, $y, $color, $puntoA, $puntoB, $puntoC) {
        parent::__construct($x, $y, $color);
        $this->puntoA = $puntoA;
        $this->puntoB = $puntoB;
        $this->puntoC = $puntoC;
    }

    // Método para calcular el área del triángulo
    public function calcularArea() {
        $base = sqrt(($this->puntoB->x - $this->puntoA->x) ** 2 + ($this->puntoB->y - $this->puntoA->y) ** 2);
        $altura = sqrt(($this->puntoC->x - $this->puntoB->x) ** 2 + ($this->puntoC->y - $this->puntoB->y) ** 2);
        return 0.5 * $base * $altura;
    }

    // Método para dibujar el triángulo
    public function dibujar() {
        echo "<polygon points=\"{$this->puntoA->x},{$this->puntoA->y} {$this->puntoB->x},{$this->puntoB->y} {$this->puntoC->x},{$this->puntoC