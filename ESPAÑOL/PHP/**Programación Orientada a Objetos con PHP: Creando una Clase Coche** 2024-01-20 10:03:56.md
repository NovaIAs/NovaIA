```php
<?php

// Definimos una clase que representa un objeto "Coche"
class Coche {

    // Propiedades del coche
    private $marca;
    private $modelo;
    private $color;
    private $kilometros;

    // Constructor de la clase
    public function __construct($marca, $modelo, $color, $kilometros) {
        $this->marca = $marca;
        $this->modelo = $modelo;
        $this->color = $color;
        $this->kilometros = $kilometros;
    }

    // Métodos de la clase

    // Método para obtener la marca del coche
    public function getMarca() {
        return $this->marca;
    }

    // Método para obtener el modelo del coche
    public function getModelo() {
        return $this->modelo;
    }

    // Método para obtener el color del coche
    public function getColor() {
        return $this->color;
    }

    // Método para obtener los kilómetros del coche
    public function getKilometros() {
        return $this->kilometros;
    }

    // Método para imprimir información sobre el coche
    public function imprimirInformacion() {
        echo "Marca: $this->marca\n";
        echo "Modelo: $this->modelo\n";
        echo "Color: $this->color\n";
        echo "Kilómetros: $this->kilometros\n";
    }

}

// Creamos un objeto "coche" de tipo Coche
$coche1 = new Coche("Toyota", "Corolla", "Rojo", 100000);

// Imprimimos información sobre el coche
echo "Información sobre el coche:\n";
$coche1->imprimirInformacion();

?>
```

Este código define una clase llamada `Coche` que representa un objeto coche con propiedades como la marca, el modelo, el color y los kilómetros. La clase tiene un constructor que inicializa estas propiedades cuando se crea un objeto de la clase. También tiene métodos para obtener cada una de las propiedades y un método para imprimir información sobre el coche.

Luego, el código crea un objeto `coche1` de tipo `Coche` y utiliza el método `imprimirInformacion()` para imprimir información sobre el coche.