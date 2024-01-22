```php
// Este código implementa una clase abstracta llamada "Shape" que define una interfaz común para diferentes formas geométricas.

abstract class Shape {
    // La propiedad "type" almacena el tipo de forma geométrica.
    protected $type;

    // Este método abstracto calcula el área de la forma geométrica.
    abstract public function calculateArea();

    // Este método abstracto calcula el perímetro de la forma geométrica.
    abstract public function calculatePerimeter();

    // Este método mágico se ejecuta cuando se crea un nuevo objeto de la clase.
    public function __construct($type) {
        $this->type = $type;
    }

    // Este método mágico se ejecuta cuando se imprime un objeto de la clase.
    public function __toString() {
        return "Tipo de forma: $this->type";
    }
}

// Esta clase extiende la clase abstracta "Shape" e implementa las operaciones para calcular el área y el perímetro de un cuadrado.

class Square extends Shape {
    // La propiedad "side" almacena el lado del cuadrado.
    protected $side;

    // Este método calcula el área del cuadrado.
    public function calculateArea() {
        return $this->side * $this->side;
    }

    // Este método calcula el perímetro del cuadrado.
    public function calculatePerimeter() {
        return 4 * $this->side;
    }

    // Este método mágico se ejecuta cuando se crea un nuevo objeto de la clase.
    public function __construct($side) {
        parent::__construct("Cuadrado");
        $this->side = $side;
    }
}

// Esta clase extiende la clase abstracta "Shape" e implementa las operaciones para calcular el área y el perímetro de un círculo.

class Circle extends Shape {
    // La propiedad "radius" almacena el radio del círculo.
    protected $radius;

    // Este método calcula el área del círculo.
    public function calculateArea() {
        return pi() * $this->radius * $this->radius;
    }

    // Este método calcula el perímetro del círculo.
    public function calculatePerimeter() {
        return 2 * pi() * $this->radius;
    }

    // Este método mágico se ejecuta cuando se crea un nuevo objeto de la clase.
    public function __construct($radius) {
        parent::__construct("Círculo");
        $this->radius = $radius;
    }
}

// Esta clase extiende la clase abstracta "Shape" e implementa las operaciones para calcular el área y el perímetro de un triángulo.

class Triangle extends Shape {
    // La propiedad "base" almacena la base del triángulo.
    protected $base;

    // La propiedad "height" almacena la altura del triángulo.
    protected $height;

    // Este método calcula el área del triángulo.
    public function calculateArea() {
        return 0.5 * $this->base * $this->height;
    }

    // Este método calcula el perímetro del triángulo.
    public function calculatePerimeter() {
        return $this->base + $this->height + sqrt($this->base * $this->base + $this->height * $this->height);
    }

    // Este método mágico se ejecuta cuando se crea un nuevo objeto de la clase.
    public function __construct($base, $height) {
        parent::__construct("Triángulo");
        $this->base = $base;
        $this->height = $height;
    }
}

// Creamos un objeto de la clase "Square".
$square = new Square(5);

// Imprimimos el tipo de forma y el área y perímetro del cuadrado.
echo "Tipo de forma: ", $square, "\n";
echo "Área: ", $square->calculateArea(), "\n";
echo "Perímetro: ", $square->calculatePerimeter(), "\n";

// Creamos un objeto de la clase "Circle".
$circle = new Circle(10);

// Imprimimos el tipo de forma y el área y perímetro del círculo.
echo "Tipo de forma: ", $circle, "\n";
echo "Área: ", $circle->calculateArea(), "\n";
echo "Perímetro: ", $circle->calculatePerimeter(), "\n";

// Creamos un objeto de la clase "Triangle".
$triangle = new Triangle(10, 15);

// Imprimimos el tipo de forma y el área y perímetro del triángulo.
echo "Tipo de forma: ", $triangle, "\n";
echo "Área: ", $triangle->calculateArea(), "\n";
echo "Perímetro: ", $triangle->calculatePerimeter(), "\n";
```

Explicación del código:

* La clase abstracta "Shape" define la interfaz común para las diferentes formas geométricas. Esta clase contiene métodos abstractos "calculateArea" y "calculatePerimeter" que deben ser implementados por las clases concretas.
* Las clases concretas "Square", "Circle" y "Triangle" extienden la clase abstracta "Shape" e implementan los métodos abstractos para calcular el área y el perímetro de cada forma geométrica específica.
* Creamos objetos de las clases concretas y calculamos el área y el perímetro de cada forma geométrica.
* Imprimimos el tipo de forma, el área y el perímetro de cada forma geométrica.

Este código es complejo y difícil de repetir porque:

* Utiliza clases abstractas e interfaces para definir una interfaz común para diferentes formas geométricas.
* Las clases concretas extienden la clase abstracta e implementan los métodos abstractos para calcular el área y el perímetro de cada forma geométrica específica.
* El código utiliza métodos mágicos para ejecutar código específico cuando se crea un nuevo objeto y cuando se imprime un objeto.
* El código calcula el área y el perímetro de diferentes formas geométricas y lo imprime en la consola.

Este código es un ejemplo de cómo se puede utilizar la herencia y la abstracción para crear una interfaz común para diferentes objetos. También muestra cómo se pueden utilizar los métodos mágicos para ejecutar código específico cuando se crea un nuevo objeto y cuando se imprime un objeto.