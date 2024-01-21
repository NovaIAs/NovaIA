```php
// Definir una clase llamada "Calculadora" que contenga varios métodos para realizar operaciones matemáticas básicas.

class Calculadora {

    // Definir un método llamado "sumar" que reciba dos números como argumentos y devuelva su suma.

    public function sumar($a, $b) {
        return $a + $b;
    }

    // Definir un método llamado "restar" que reciba dos números como argumentos y devuelva su diferencia.

    public function restar($a, $b) {
        return $a - $b;
    }

    // Definir un método llamado "multiplicar" que reciba dos números como argumentos y devuelva su producto.

    public function multiplicar($a, $b) {
        return $a * $b;
    }

    // Definir un método llamado "dividir" que reciba dos números como argumentos y devuelva su cociente.

    public function dividir($a, $b) {
        if ($b == 0) {
            throw new DivisionByZeroError("No se puede dividir por cero.");
        }
        return $a / $b;
    }

    // Definir un método llamado "potencia" que reciba dos números como argumentos y devuelva el primero elevado al segundo.

    public function potencia($a, $b) {
        return pow($a, $b);
    }

    // Definir un método llamado "raiz" que reciba un número como argumento y devuelva su raíz cuadrada.

    public function raiz($a) {
        return sqrt($a);
    }

    // Definir un método llamado "seno" que reciba un número como argumento y devuelva el seno de ese número.

    public function seno($a) {
        return sin($a);
    }

    // Definir un método llamado "coseno" que reciba un número como argumento y devuelva el coseno de ese número.

    public function coseno($a) {
        return cos($a);
    }

    // Definir un método llamado "tangente" que reciba un número como argumento y devuelva la tangente de ese número.

    public function tangente($a) {
        return tan($a);
    }
}

// Crear una instancia de la clase "Calculadora".

$calculadora = new Calculadora();

// Utilizar los métodos de la clase "Calculadora" para realizar algunas operaciones matemáticas.

$suma = $calculadora->sumar(1, 2);
$resta = $calculadora->restar(3, 4);
$multiplicacion = $calculadora->multiplicar(5, 6);
$division = $calculadora->dividir(7, 8);
$potencia = $calculadora->potencia(9, 2);
$raiz = $calculadora->raiz(10);
$seno = $calculadora->seno(30);
$coseno = $calculadora->coseno(45);
$tangente = $calculadora->tangente(60);

// Mostrar los resultados de las operaciones matemáticas.

echo "Suma: $suma\n";
echo "Resta: $resta\n";
echo "Multiplicación: $multiplicacion\n";
echo "División: $division\n";
echo "Potencia: $potencia\n";
echo "Raíz: $raiz\n";
echo "Seno: $seno\n";
echo "Coseno: $coseno\n";
echo "Tangente: $tangente\n";
```

Explicación:

1. Primero, definimos una clase llamada "Calculadora" que contiene varios métodos para realizar operaciones matemáticas básicas.
2. Luego, creamos una instancia de la clase "Calculadora" utilizando la palabra clave `new`.
3. A continuación, utilizamos los métodos de la clase "Calculadora" para realizar algunas operaciones matemáticas.
4. Finalmente, mostramos los resultados de las operaciones matemáticas utilizando la función `echo`.