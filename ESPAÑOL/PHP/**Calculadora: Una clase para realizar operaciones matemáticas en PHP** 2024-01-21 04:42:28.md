```php
// Definición de la clase
class Calculadora {

  // Función para sumar dos números
  public function sumar($a, $b) {
    return $a + $b;
  }

  // Función para restar dos números
  public function restar($a, $b) {
    return $a - $b;
  }

  // Función para multiplicar dos números
  public function multiplicar($a, $b) {
    return $a * $b;
  }

  // Función para dividir dos números
  public function dividir($a, $b) {
    if ($b == 0) {
      throw new Exception("No se puede dividir por cero");
    }
    return $a / $b;
  }

  // Función para calcular la raíz cuadrada de un número
  public function raizCuadrada($a) {
    if ($a < 0) {
      throw new Exception("No se puede calcular la raíz cuadrada de un número negativo");
    }
    return sqrt($a);
  }

}

// Uso de la clase
$calculadora = new Calculadora();

// Ejemplo de suma
$resultado = $calculadora->sumar(5, 10);
echo "La suma de 5 y 10 es $resultado\n";

// Ejemplo de resta
$resultado = $calculadora->restar(10, 5);
echo "La resta de 10 y 5 es $resultado\n";

// Ejemplo de multiplicación
$resultado = $calculadora->multiplicar(5, 10);
echo "La multiplicación de 5 y 10 es $resultado\n";

// Ejemplo de división
try {
  $resultado = $calculadora->dividir(10, 2);
  echo "La división de 10 y 2 es $resultado\n";
} catch (Exception $e) {
  echo "No se puede dividir por cero\n";
}

// Ejemplo de raíz cuadrada
try {
  $resultado = $calculadora->raizCuadrada(25);
  echo "La raíz cuadrada de 25 es $resultado\n";
} catch (Exception $e) {
  echo "No se puede calcular la raíz cuadrada de un número negativo\n";
}
```

Explicación del código:

* La clase `Calculadora` define cinco métodos: `sumar`, `restar`, `multiplicar`, `dividir` y `raizCuadrada`. Cada método realiza una operación matemática diferente.
* Los métodos `sumar`, `restar` y `multiplicar` son sencillos y no necesitan explicación adicional.
* El método `dividir` comprueba si el segundo parámetro es cero y lanza una excepción si lo es. Esto se hace para evitar la división por cero, que es un error matemático.
* El método `raizCuadrada` comprueba si el parámetro es negativo y lanza una excepción si lo es. Esto se hace porque la raíz cuadrada de un número negativo no es un número real.
* La clase `Calculadora` se instancia y se utilizan sus métodos para realizar diferentes operaciones matemáticas.
* Los resultados de las operaciones se muestran en la salida estándar.