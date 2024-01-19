```d
// Código complejo en D

// Definimos una clase llamada "Complejo" para representar números complejos.
class Complejo {
    real, imag; // Partes real e imaginaria del número complejo

    // Constructor que toma dos parámetros: la parte real y la imaginaria.
    this(real, imag) {
        this.real = real;
        this.imag = imag;
    }

    // Sobrecarga del operador "+" para sumar dos números complejos.
    Complejo opAdd(Complejo otro) {
        return new Complejo(this.real + otro.real, this.imag + otro.imag);
    }

    // Sobrecarga del operador "-" para restar dos números complejos.
    Complejo opSub(Complejo otro) {
        return new Complejo(this.real - otro.real, this.imag - otro.imag);
    }

    // Sobrecarga del operador "*" para multiplicar dos números complejos.
    Complejo opMul(Complejo otro) {
        return new Complejo(this.real * otro.real - this.imag * otro.imag,
            this.real * otro.imag + this.imag * otro.real);
    }

    // Sobrecarga del operador "/" para dividir dos números complejos.
    Complejo opDiv(Complejo otro) {
        double denominador = otro.real * otro.real + otro.imag * otro.imag;
        return new Complejo((this.real * otro.real + this.imag * otro.imag) / denominador,
            (this.imag * otro.real - this.real * otro.imag) / denominador);
    }

    // Método para obtener la parte real del número complejo.
    double getReal() {
        return this.real;
    }

    // Método para obtener la parte imaginaria del número complejo.
    double getImag() {
        return this.imag;
    }

    // Método para obtener el módulo del número complejo.
    double getModulo() {
        return Math.sqrt(this.real * this.real + this.imag * this.imag);
    }

    // Método para obtener el argumento del número complejo.
    double getArgumento() {
        return Math.atan2(this.imag, this.real);
    }

    // Sobrecarga del operador "toString" para imprimir el número complejo en formato "(real, imag)".
    string toString() {
        return "({this.real}, {this.imag})";
    }
}

// Creamos dos números complejos.
Complejo z1 = new Complejo(1, 2);
Complejo z2 = new Complejo(3, 4);

// Sumamos los dos números complejos.
Complejo suma = z1 + z2;

// Restamos los dos números complejos.
Complejo resta = z1 - z2;

// Multiplicamos los dos números complejos.
Complejo producto = z1 * z2;

// Dividimos los dos números complejos.
Complejo cociente = z1 / z2;

// Imprimimos los resultados.
println("Suma: {suma}");
println("Resta: {resta}");
println("Producto: {producto}");
println("Cociente: {cociente}");
```

Explicación del código:

* La clase `Complejo` representa números complejos. Tiene dos propiedades: `real` y `imag`.
* Los operadores `+`, `-`, `*` y `/` están sobrecargados para realizar las operaciones aritméticas básicas entre números complejos.
* Los métodos `getReal()`, `getImag()`, `getModulo()` y `getArgumento()` obtienen la parte real, la parte imaginaria, el módulo y el argumento del número complejo, respectivamente.
* El método `toString()` devuelve una representación en cadena del número complejo en formato "(real, imag)".
* Creamos dos números complejos `z1` y `z2`.
* Sumamos, restamos, multiplicamos y dividimos los dos números complejos.
* Imprimimos los resultados.