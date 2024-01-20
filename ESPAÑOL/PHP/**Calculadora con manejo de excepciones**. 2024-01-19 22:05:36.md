```php
<?php
// Definición de clase
class Calculadora {
  // Propiedades
  private $numero1;
  private $numero2;

  // Constructor
  public function __construct($numero1, $numero2) {
    $this->numero1 = $numero1;
    $this->numero2 = $numero2;
  }

  // Métodos
  public function sumar() {
    return $this->numero1 + $this->numero2;
  }

  public function restar() {
    return $this->numero1 - $this->numero2;
  }

  public function multiplicar() {
    return $this->numero1 * $this->numero2;
  }

  public function dividir() {
    if ($this->numero2 != 0) {
      return $this->numero1 / $this->numero2;
    } else {
      throw new Exception("No se puede dividir por cero.");
    }
  }
}

// Creación de objeto
$calculadora = new Calculadora(10, 5);

// Uso de los métodos
echo "Suma: " . $calculadora->sumar() . "\n";
echo "Resta: " . $calculadora->restar() . "\n";
echo "Multiplicación: " . $calculadora->multiplicar() . "\n";
try {
  echo "División: " . $calculadora->dividir() . "\n";
} catch (Exception $e) {
  echo "Error: " . $e->getMessage() . "\n";
}
?>
```

Explicación:

* **Definición de clase**: Se define una clase llamada `Calculadora` que representa una calculadora simple.
* **Propiedades**: La clase `Calculadora` tiene dos propiedades privadas llamadas `numero1` y `numero2` que se utilizan para almacenar los dos números que se van a calcular.
* **Constructor**: La clase `Calculadora` tiene un constructor que toma dos parámetros, `numero1` y `numero2`, y los asigna a las propiedades privadas de la clase.
* **Métodos**: La clase `Calculadora` tiene cuatro métodos públicos llamados `sumar()`, `restar()`, `multiplicar()` y `dividir()`. Cada uno de estos métodos realiza una operación matemática diferente en los dos números almacenados en las propiedades privadas de la clase.
* **Creación de objeto**: Se crea un objeto de la clase `Calculadora` pasando dos números como argumentos al constructor.
* **Uso de los métodos**: Se utilizan los métodos del objeto `calculadora` para realizar cálculos y mostrar los resultados en la consola.
* **Manejo de excepciones**: El método `dividir()` se utiliza para dividir el `numero1` por el `numero2`. Si el `numero2` es igual a cero, se lanza una excepción y se muestra un mensaje de error.