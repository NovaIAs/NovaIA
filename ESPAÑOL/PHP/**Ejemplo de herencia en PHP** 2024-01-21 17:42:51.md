```php
<?php

// Definir las clases

class Vehiculo {
  protected $marca;
  protected $modelo;
  protected $año;

  public function __construct($marca, $modelo, $año) {
    $this->marca = $marca;
    $this->modelo = $modelo;
    $this->año = $año;
  }

  public function getMarca() {
    return $this->marca;
  }

  public function getModelo() {
    return $this->modelo;
  }

  public function getAño() {
    return $this->año;
  }
}

class Coche extends Vehiculo {
  protected $numPuertas;

  public function __construct($marca, $modelo, $año, $numPuertas) {
    parent::__construct($marca, $modelo, $año);
    $this->numPuertas = $numPuertas;
  }

  public function getNumPuertas() {
    return $this->numPuertas;
  }
}

class Moto extends Vehiculo {
  protected $cilindrada;

  public function __construct($marca, $modelo, $año, $cilindrada) {
    parent::__construct($marca, $modelo, $año);
    $this->cilindrada = $cilindrada;
  }

  public function getCilindrada() {
    return $this->cilindrada;
  }
}

// Crear objetos

$coche1 = new Coche("Toyota", "Yaris", 2015, 5);
$moto1 = new Moto("Honda", "CBR600RR", 2018, 600);

// Mostrar información de los objetos

echo "Coche 1:\n";
echo "Marca: " . $coche1->getMarca() . "\n";
echo "Modelo: " . $coche1->getModelo() . "\n";
echo "Año: " . $coche1->getAño() . "\n";
echo "Número de puertas: " . $coche1->getNumPuertas() . "\n\n";

echo "Moto 1:\n";
echo "Marca: " . $moto1->getMarca() . "\n";
echo "Modelo: " . $moto1->getModelo() . "\n";
echo "Año: " . $moto1->getAño() . "\n";
echo "Cilindrada: " . $moto1->getCilindrada() . "cc\n";

?>
```

Este código define dos clases: `Vehiculo` y `Coche`. La clase `Vehiculo` es la clase base, y la clase `Coche` es una clase derivada de `Vehiculo`.

La clase `Vehiculo` tiene tres propiedades protegidas: `marca`, `modelo` y `año`. Estas propiedades se utilizan para almacenar la marca, el modelo y el año de un vehículo. La clase `Vehiculo` también tiene tres métodos públicos: `getMarca()`, `getModelo()` y `getAño()`. Estos métodos se utilizan para obtener el valor de las propiedades correspondientes.

La clase `Coche` hereda las propiedades y los métodos de la clase `Vehiculo`. Además, la clase `Coche` tiene una propiedad protegida adicional: `numPuertas`. Esta propiedad se utiliza para almacenar el número de puertas de un coche. La clase `Coche` también tiene un método público adicional: `getNumPuertas()`. Este método se utiliza para obtener el valor de la propiedad `numPuertas`.

El código también crea dos objetos: `coche1` y `moto1`. El objeto `coche1` es un objeto de la clase `Coche`, y el objeto `moto1` es un objeto de la clase `Moto`.

El código muestra la información de los objetos `coche1` y `moto1` utilizando los métodos `getMarca()`, `getModelo()`, `getAño()`, `getNumPuertas()` y `getCilindrada()`.