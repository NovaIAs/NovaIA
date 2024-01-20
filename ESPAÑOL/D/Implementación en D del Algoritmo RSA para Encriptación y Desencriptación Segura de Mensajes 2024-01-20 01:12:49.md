```d
// Importamos las librerías necesarias.
import std.bigint;
import std.stdio;
import std.algorithm;

// Definimos una función para calcular el factorial de un número.
double factorial(double n) {
    if (n == 0) {
        return 1;
    } else {
        return n * factorial(n - 1);
    }
}

// Definimos una función para calcular el coeficiente binomial de dos números.
double binomialCoefficient(double n, double k) {
    return factorial(n) / (factorial(k) * factorial(n - k));
}

// Definimos una función para generar un número primo aleatorio.
double generatePrimeNumber() {
    // Creamos una lista de números primos.
    auto primes = [2];

    // Iteramos sobre los números impares hasta encontrar un número primo.
    for (double i = 3; i < 1000000; i += 2) {
        // Comprobamos si el número es divisible por alguno de los números primos conocidos.
        auto isPrime = true;
        for (auto prime : primes) {
            if (i % prime == 0) {
                isPrime = false;
                break;
            }
        }

        // Si el número es primo, lo añadimos a la lista de números primos y lo devolvemos.
        if (isPrime) {
            primes.add(i);
            return i;
        }
    }

    // Si no encontramos ningún número primo, devolvemos 0.
    return 0;
}

// Definimos una función para encriptar un mensaje usando el algoritmo RSA.
string encryptRSA(string message, double e, double n) {
    // Convertimos el mensaje a una lista de enteros.
    auto messageIntegers = message.split(' ');

    // Encriptamos cada entero del mensaje usando el algoritmo RSA.
    auto encryptedMessageIntegers = messageIntegers.map!(integer) integer.powm(e, n);

    // Convertimos la lista de enteros encriptados a una cadena de texto.
    auto encryptedMessage = encryptedMessageIntegers.join(' ');

    // Devolvemos el mensaje encriptado.
    return encryptedMessage;
}

// Definimos una función para desencriptar un mensaje usando el algoritmo RSA.
string decryptRSA(string encryptedMessage, double d, double n) {
    // Convertimos el mensaje encriptado a una lista de enteros.
    auto encryptedMessageIntegers = encryptedMessage.split(' ');

    // Desencriptamos cada entero del mensaje encriptado usando el algoritmo RSA.
    auto decryptedMessageIntegers = encryptedMessageIntegers.map!(integer) integer.powm(d, n);

    // Convertimos la lista de enteros desencriptados a una cadena de texto.
    auto decryptedMessage = decryptedMessageIntegers.join(' ');

    // Devolvemos el mensaje desencriptado.
    return decryptedMessage;
}

// Definimos la función principal del programa.
void main() {
    // Generamos dos números primos aleatorios.
    auto p = generatePrimeNumber();
    auto q = generatePrimeNumber();

    // Calculamos el valor de n.
    auto n = p * q;

    // Calculamos el valor de φ(n).
    auto phiN = (p - 1) * (q - 1);

    // Elegimos un valor de e que sea coprimo con φ(n).
    auto e = 3;
    while (e < phiN && std.gcd(e, phiN) != 1) {
        e++;
    }

    // Calculamos el valor de d que sea el inverso modular de e módulo φ(n).
    auto d = e.modInverse(phiN);

    // Pedimos al usuario que introduzca el mensaje que quiere encriptar.
    write("Introduce el mensaje que quieres encriptar: ");
    auto message = readln();

    // Encriptamos el mensaje usando el algoritmo RSA.
    auto encryptedMessage = encryptRSA(message, e, n);

    // Mostramos el mensaje encriptado.
    write("Mensaje encriptado: ", encryptedMessage, "\n");

    // Desencriptamos el mensaje usando el algoritmo RSA.
    auto decryptedMessage = decryptRSA(encryptedMessage, d, n);

    // Mostramos el mensaje desencriptado.
    write("Mensaje desencriptado: ", decryptedMessage, "\n");
}
```

Este código implementa el algoritmo RSA, que es un algoritmo de criptografía asimétrica. El algoritmo RSA utiliza dos números primos grandes, p y q, para generar dos claves: una clave pública y una clave privada. La clave pública se utiliza para encriptar mensajes, y la clave privada se utiliza para desencriptar mensajes.

El código primero genera dos números primos aleatorios, p y q, y luego calcula el valor de n. El valor de n se utiliza para generar las dos claves.

El siguiente paso es elegir un valor de e que sea coprimo con φ(n). El valor de e se utiliza para encriptar mensajes.

El siguiente paso es calcular el valor de d que sea el inverso modular de e módulo φ(n). El valor de d se utiliza para desencriptar mensajes.

El usuario introduce el mensaje que quiere encriptar, y el código encripta el mensaje usando el algoritmo RSA.

El mensaje encriptado se muestra en la consola.

El usuario introduce la clave privada, y el código desencripta el mensaje usando el algoritmo RSA.

El mensaje desencriptado se muestra en la consola.