**Código:**

```php
<?php

// Función para generar cadenas aleatorias
function generarCadenaAleatoria($longitud) {
  $caracteres = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $cadenaAleatoria = '';
  for ($i = 0; $i < $longitud; $i++) {
    $cadenaAleatoria .= $caracteres[rand(0, strlen($caracteres) - 1)];
  }
  return $cadenaAleatoria;
}

// Función para cifrar datos utilizando el algoritmo AES-256
function cifrarDatos($datos, $clave) {
  $cifrado = openssl_encrypt($datos, 'AES-256-CBC', $clave, OPENSSL_RAW_DATA);
  return base64_encode($cifrado);
}

// Función para descifrar datos utilizando el algoritmo AES-256
function descifrarDatos($datosCifrados, $clave) {
  $datosDescifrados = openssl_decrypt(base64_decode($datosCifrados), 'AES-256-CBC', $clave, OPENSSL_RAW_DATA);
  return $datosDescifrados;
}

// Generación de una clave aleatoria de 32 caracteres
$clave = generarCadenaAleatoria(32);

// Cifrado de un mensaje de texto
$mensaje = '¡Hola, mundo!';
$mensajeCifrado = cifrarDatos($mensaje, $clave);

// Descifrado del mensaje de texto cifrado
$mensajeDescifrado = descifrarDatos($mensajeCifrado, $clave);

// Impresión del mensaje original y del mensaje cifrado y descifrado
echo "Mensaje original: $mensaje\n";
echo "Mensaje cifrado: $mensajeCifrado\n";
echo "Mensaje descifrado: $mensajeDescifrado\n";

?>
```

**Explicación:**

1. Definimos funciones para generar cadenas aleatorias, cifrar datos utilizando el algoritmo AES-256 y descifrar datos utilizando el algoritmo AES-256.
2. Generamos una clave aleatoria de 32 caracteres utilizando la función `generarCadenaAleatoria()`.
3. Ciframos un mensaje de texto utilizando la función `cifrarDatos()` con la clave generada anteriormente.
4. Desciframos el mensaje de texto cifrado utilizando la función `descifrarDatos()` con la misma clave que utilizamos para cifrarlo.
5. Imprimimos el mensaje original, el mensaje cifrado y el mensaje descifrado para comprobar que el proceso de cifrado y descifrado ha sido exitoso.

Este código es complejo porque utiliza varias funciones y algoritmos para generar una clave aleatoria, cifrar y descifrar datos. Además, también es diferente porque utiliza el algoritmo AES-256, que es un algoritmo de cifrado seguro y ampliamente utilizado.