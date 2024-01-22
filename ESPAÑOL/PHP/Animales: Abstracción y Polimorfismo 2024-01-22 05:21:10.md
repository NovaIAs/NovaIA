```php
<?php

// Definir una clase abstracta para representar un animal.
abstract class Animal
{
    // Propiedades comunes a todos los animales.
    protected $nombre;
    protected $especie;

    // Constructor para inicializar las propiedades.
    public function __construct($nombre, $especie)
    {
        $this->nombre = $nombre;
        $this->especie = $especie;
    }

    // Método abstracto para obtener el sonido del animal.
    abstract public function getSonido();

    // Método para obtener el nombre del animal.
    public function getNombre()
    {
        return $this->nombre;
    }

    // Método para obtener la especie del animal.
    public function getEspecie()
    {
        return $this->especie;
    }
}

// Definir una clase concreta para representar un perro.
class Perro extends Animal
{
    // Sobrescribir el método abstracto para obtener el sonido del perro.
    public function getSonido()
    {
        return "Guau!";
    }
}

// Definir una clase concreta para representar un gato.
class Gato extends Animal
{
    // Sobrescribir el método abstracto para obtener el sonido del gato.
    public function getSonido()
    {
        return "Miau!";
    }
}

// Definir una clase concreta para representar una vaca.
class Vaca extends Animal
{
    // Sobrescribir el método abstracto para obtener el sonido de la vaca.
    public function getSonido()
    {
        return "Muuu!";
    }
}

// Crear una matriz de animales.
$animales = [
    new Perro("Fido", "Perro"),
    new Gato("Misha", "Gato"),
    new Vaca("Daisy", "Vaca")
];

// Recorrer la matriz e imprimir el nombre, la especie y el sonido de cada animal.
foreach ($animales as $animal) {
    echo "Nombre: " . $animal->getNombre() . "\n";
    echo "Especie: " . $animal->getEspecie() . "\n";
    echo "Sonido: " . $animal->getSonido() . "\n";
    echo "\n";
}

?>
```