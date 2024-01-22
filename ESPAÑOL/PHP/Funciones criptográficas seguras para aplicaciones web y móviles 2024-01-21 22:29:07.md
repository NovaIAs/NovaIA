```php
<?php

// Definimos una función para generar un número aleatorio entre dos valores.
function generarNumeroAleatorio($minimo, $maximo) {
  return rand($minimo, $maximo);
}

// Definimos una función para generar una cadena aleatoria de una longitud determinada.
function generarCadenaAleatoria($longitud) {
  $caracteres = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  $cadena = '';
  for ($i = 0; $i < $longitud; $i++) {
    $cadena .= $caracteres[generarNumeroAleatorio(0, strlen($caracteres) - 1)];
  }
  return $cadena;
}

// Definimos una función para generar una contraseña segura.
function generarContraseñaSegura($longitud) {
  $caracteres = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()';
  $contraseña = '';
  for ($i = 0; $i < $longitud; $i++) {
    $contraseña .= $caracteres[generarNumeroAleatorio(0, strlen($caracteres) - 1)];
  }
  return $contraseña;
}

// Definimos una función para generar un token de acceso seguro.
function generarTokenAccesoSeguro() {
  return generarCadenaAleatoria(32);
}

// Definimos una función para generar un token de actualización seguro.
function generarTokenActualizaciónSeguro() {
  return generarCadenaAleatoria(16);
}

// Definimos una función para generar un identificador único seguro.
function generarIdentificadorUnicoSeguro() {
  return generarCadenaAleatoria(128);
}

// Definimos una función para generar un hash seguro de una cadena.
function generarHashSeguro($cadena) {
  return hash('sha256', $cadena);
}

// Definimos una función para comparar una cadena con un hash seguro.
function compararHashSeguro($cadena, $hash) {
  return hash_equals($hash, generarHashSeguro($cadena));
}

// Definimos una función para encriptar una cadena.
function encriptarCadena($cadena, $clave) {
  $iv = generarCadenaAleatoria(16);
  $encriptacion = openssl_encrypt($cadena, 'aes-256-cbc', $clave, 0, $iv);
  return $iv . $encriptacion;
}

// Definimos una función para desencriptar una cadena.
function desencriptarCadena($cadena, $clave) {
  $iv = substr($cadena, 0, 16);
  $encriptacion = substr($cadena, 16);
  return openssl_decrypt($encriptacion, 'aes-256-cbc', $clave, 0, $iv);
}

// Definimos una función para firmar una cadena.
function firmarCadena($cadena, $clavePrivada) {
  $firma = '';
  openssl_sign($cadena, $firma, $clavePrivada, OPENSSL_ALGO_SHA256);
  return $firma;
}

// Definimos una función para verificar una firma.
function verificarFirma($cadena, $firma, $clavePublica) {
  $resultado = openssl_verify($cadena, $firma, $clavePublica, OPENSSL_ALGO_SHA256);
  return $resultado === 1;
}

?>
```

Explicación del código:

Este código complejo en PHP proporciona una variedad de funciones para generar valores aleatorios seguros, contraseñas seguras, tokens de acceso y actualización seguros, identificadores únicos seguros, hashes seguros, cadenas encriptadas, firmas digitales y verificar firmas digitales. Estas funciones pueden ser utilizadas para mejorar la seguridad de aplicaciones web y móviles.

* La función **generarNumeroAleatorio** genera un número aleatorio entre dos valores especificados.
* La función **generarCadenaAleatoria** genera una cadena aleatoria de una longitud especificada.
* La función **generarContraseñaSegura** genera una contraseña segura de una longitud especificada.
* La función **generarTokenAccesoSeguro** genera un token de acceso seguro.
* La función **generarTokenActualizaciónSeguro** genera un token de actualización seguro.
* La función **generarIdentificadorUnicoSeguro** genera un identificador único seguro.
* La función **generarHashSeguro** genera un hash seguro de una cadena.
* La función **compararHashSeguro** compara una cadena con un hash seguro.
* La función **encriptarCadena** encripta una cadena con una clave especificada.
* La función **desencriptarCadena** desencripta una cadena con una clave especificada.
* La función **firmarCadena** firma una cadena con una clave privada especificada.
* La función **verificarFirma** verifica una firma con una clave pública especificada.

Estas funciones pueden utilizarse para mejorar la seguridad de aplicaciones web y móviles de las siguientes maneras:

* Las contraseñas seguras pueden utilizarse para proteger las cuentas de usuario de ataques de fuerza bruta.
* Los tokens de acceso y actualización seguros pueden utilizarse para proteger las sesiones de usuario de ataques de secuestro de sesión.
* Los identificadores únicos seguros pueden utilizarse para generar identificadores únicos para los recursos de la aplicación, como los usuarios, los productos y las transacciones.
* Los hashes seguros pueden utilizarse para almacenar contraseñas y otros datos sensibles de forma segura en la base de datos.
* Las cadenas encriptadas pueden utilizarse para almacenar datos sensibles en archivos o en la nube de forma segura.
* Las firmas digitales pueden utilizarse para verificar la integridad de los datos transmitidos entre dos partes.

Estas funciones se pueden utilizar de forma individual o combinada para crear aplicaciones web y móviles más seguras.