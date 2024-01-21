```php
<?php

// Definir una clase abstracta para representar un animal.
abstract class Animal {
  // Definir variables privadas para almacenar los atributos del animal.
  private $nombre;
  private $especie;
  private $edad;

  // Definir el constructor de la clase para inicializar los atributos.
  public function __construct($nombre, $especie, $edad) {
    $this->nombre = $nombre;
    $this->especie = $especie;
    $this->edad = $edad;
  }

  // Definir un método abstracto para obtener el sonido del animal.
  abstract public function getSonido();

  // Definir un método para obtener el nombre del animal.
  public function getNombre() {
    return $this->nombre;
  }

  // Definir un método para obtener la especie del animal.
  public function getEspecie() {
    return $this->especie;
  }

  // Definir un método para obtener la edad del animal.
  public function getEdad() {
    return $this->edad;
  }
}

// Definir una clase concreta para representar un perro.
class Perro extends Animal {
  // Definir el método getSonido() para devolver el sonido del perro.
  public function getSonido() {
    return "Guau, guau!";
  }
}

// Definir una clase concreta para representar un gato.
class Gato extends Animal {
  // Definir el método getSonido() para devolver el sonido del gato.
  public function getSonido() {
    return "Miau, miau!";
  }
}

// Definir una clase concreta para representar un loro.
class Loro extends Animal {
  // Definir el método getSonido() para devolver el sonido del loro.
  public function getSonido() {
    return "Polly quiere una galleta!";
  }
}

// Crear un array de objetos animales.
$animales = array(
  new Perro("Toby", "Labrador Retriever", 5),
  new Gato("Kitty", "Siamés", 3),
  new Loro("Polly", "Guacamayo azul y dorado", 10)
);

// Recorrer el array de objetos animales y mostrar el sonido de cada animal.
foreach ($animales as $animal) {
  echo $animal->getNombre() . " dice " . $animal->getSonido() . "\n";
}

?>
```

Este código define una clase abstracta `Animal` que representa un animal genérico. La clase tiene tres variables privadas para almacenar el nombre, la especie y la edad del animal. También define un método abstracto `getSonido()` que debe ser implementado por las clases concretas que hereden de `Animal`.

A continuación, se definen tres clases concretas: `Perro`, `Gato` y `Loro`. Cada una de estas clases implementa el método `getSonido()` para devolver el sonido correspondiente al animal.

Por último, se crea un array de objetos animales y se recorre el array para mostrar el sonido de cada animal.

La salida del código será:

```
Toby dice Guau, guau!
Kitty dice Miau, miau!
Polly dice Polly quiere una galleta!
```