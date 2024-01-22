```php
<?php

// Definición de función
function calculadoraCompleja($operacion, $num1, $num2) {
  switch ($operacion) {
    case 'suma':
      return $num1 + $num2;
      break;
    case 'resta':
      return $num1 - $num2;
      break;
    case 'multiplicación':
      return $num1 * $num2;
      break;
    case 'división':
      return $num1 / $num2;
      break;
    case 'potencia':
      return pow($num1, $num2);
      break;
    case 'raíz cuadrada':
      return sqrt($num1);
      break;
    default:
      return "Operación no reconocida.";
      break;
  }
}

// Definición de clase
class CalculadoraCompleja {
  private $operacion;
  private $num1;
  private $num2;

  public function __construct($operacion, $num1, $num2) {
    $this->operacion = $operacion;
    $this->num1 = $num1;
    $this->num2 = $num2;
  }

  public function calcular() {
    switch ($this->operacion) {
      case 'suma':
        return $this->num1 + $this->num2;
        break;
      case 'resta':
        return $this->num1 - $this->num2;
        break;
      case 'multiplicación':
        return $this->num1 * $this->num2;
        break;
      case 'división':
        return $this->num1 / $this->num2;
        break;
      case 'potencia':
        return pow($this->num1, $this->num2);
        break;
      case 'raíz cuadrada':
        return sqrt($this->num1);
        break;
      default:
        return "Operación no reconocida.";
        break;
    }
  }
}

// Ejemplo de uso de la función
$resultado = calculadoraCompleja('suma', 5, 10);
echo "El resultado de la suma es: " . $resultado . "\n";

// Ejemplo de uso de la clase
$calculadora = new CalculadoraCompleja('potencia', 2, 3);
$resultado = $calculadora->calcular();
echo "El resultado de la potencia es: " . $resultado . "\n";

?>
```

Explicación del código:

1. Se define una función llamada `calculadoraCompleja` que recibe tres parámetros: `$operacion`, `$num1` y `$num2`. Esta función realiza una operación matemática en función del primer parámetro, utilizando los otros dos parámetros como operandos.

2. La función utiliza una instrucción `switch` para determinar qué operación realizar en función del valor del primer parámetro. Se admiten varias operaciones, incluyendo suma, resta, multiplicación, división, potencia y raíz cuadrada.

3. Se define una clase llamada `CalculadoraCompleja` que tiene tres propiedades privadas: `$operacion`, `$num1` y `$num2`. Estas propiedades se utilizan para almacenar los valores de los parámetros pasados a la función `calculadoraCompleja`.

4. La clase `CalculadoraCompleja` tiene un constructor que se llama automáticamente cuando se crea un nuevo objeto de la clase. El constructor recibe los mismos tres parámetros que la función `calculadoraCompleja` y los asigna a las propiedades privadas.

5. La clase `CalculadoraCompleja` también tiene un método llamado `calcular` que realiza la operación matemática en función del valor de la propiedad `$operacion`. El método utiliza una instrucción `switch` similar a la de la función `calculadoraCompleja`.

6. Se proporcionan dos ejemplos de uso del código. El primer ejemplo utiliza la función `calculadoraCompleja` para realizar una suma, mientras que el segundo ejemplo utiliza la clase `CalculadoraCompleja` para realizar una potencia.