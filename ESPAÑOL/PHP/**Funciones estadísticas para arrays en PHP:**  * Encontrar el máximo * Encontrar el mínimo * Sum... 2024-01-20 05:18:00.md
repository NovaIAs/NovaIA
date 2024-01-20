```php
// Crear una función que reciba un array de números y devuelva el mayor
function encontrarMaximo(array $numeros)
{
    // Si el array está vacío, devolver null
    if (empty($numeros)) {
        return null;
    }

    // Inicializar el máximo con el primer elemento del array
    $maximo = $numeros[0];

    // Recorrer el array y actualizar el máximo si es necesario
    foreach ($numeros as $numero) {
        if ($numero > $maximo) {
            $maximo = $numero;
        }
    }

    // Devolver el máximo
    return $maximo;
}

// Crear una función que reciba un array de números y devuelva el menor
function encontrarMinimo(array $numeros)
{
    // Si el array está vacío, devolver null
    if (empty($numeros)) {
        return null;
    }

    // Inicializar el mínimo con el primer elemento del array
    $minimo = $numeros[0];

    // Recorrer el array y actualizar el mínimo si es necesario
    foreach ($numeros as $numero) {
        if ($numero < $minimo) {
            $minimo = $numero;
        }
    }

    // Devolver el mínimo
    return $minimo;
}

// Crear una función que reciba un array de números y devuelva la suma de todos ellos
function sumarArray(array $numeros)
{
    // Si el array está vacío, devolver 0
    if (empty($numeros)) {
        return 0;
    }

    // Inicializar la suma con 0
    $suma = 0;

    // Recorrer el array y sumar cada elemento a la suma
    foreach ($numeros as $numero) {
        $suma += $numero;
    }

    // Devolver la suma
    return $suma;
}

// Crear una función que reciba un array de números y devuelva el producto de todos ellos
function multiplicarArray(array $numeros)
{
    // Si el array está vacío, devolver 1
    if (empty($numeros)) {
        return 1;
    }

    // Inicializar el producto con 1
    $producto = 1;

    // Recorrer el array y multiplicar cada elemento por el producto
    foreach ($numeros as $numero) {
        $producto *= $numero;
    }

    // Devolver el producto
    return $producto;
}

// Crear una función que reciba un array de números y devuelva la media de todos ellos
function calcularMedia(array $numeros)
{
    // Si el array está vacío, devolver null
    if (empty($numeros)) {
        return null;
    }

    // Calcular la suma de los números
    $suma = sumarArray($numeros);

    // Calcular la longitud del array
    $longitud = count($numeros);

    // Devolver la media
    return $suma / $longitud;
}

// Crear una función que reciba un array de números y devuelva la desviación estándar de todos ellos
function calcularDesviacionEstandar(array $numeros)
{
    // Si el array está vacío, devolver null
    if (empty($numeros)) {
        return null;
    }

    // Calcular la media de los números
    $media = calcularMedia($numeros);

    // Calcular la suma de las desviaciones cuadráticas de la media
    $sumaDesviaciones = 0;
    foreach ($numeros as $numero) {
        $desviacion = $numero - $media;
        $sumaDesviaciones += $desviacion * $desviacion;
    }

    // Calcular la desviación estándar
    $desviacionEstandar = sqrt($sumaDesviaciones / (count($numeros) - 1));

    // Devolver la desviación estándar
    return $desviacionEstandar;
}
```

Este código contiene una serie de funciones estadísticas útiles. Las funciones son:

* **encontrarMaximo**: encuentra el número mayor en un array.
* **encontrarMinimo**: encuentra el número menor en un array.
* **sumarArray**: suma todos los números en un array.
* **multiplicarArray**: multiplica todos los números en un array.
* **calcularMedia**: calcula la media de los números en un array.
* **calcularDesviacionEstandar**: calcula la desviación estándar de los números en un array.

Las funciones están escritas en PHP y son relativamente fáciles de entender. Se pueden utilizar para realizar cálculos estadísticos básicos sobre un array de números.