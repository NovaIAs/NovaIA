```d
// Módulo que define una clase compleja para representar números complejos.

module complex;

import std.stdio;

// Definición de la clase complex.
class complex {
    // Atributos privados para representar la parte real e imaginaria.
    private real re;
    private real im;

public:

    // Constructor por defecto que inicializa el número complejo a 0 + 0i.
    this() pure, nothrow {
        re = 0;
        im = 0;
    }

    // Constructor que inicializa el número complejo con la parte real y imaginaria dadas.
    this(real re, real im) pure, nothrow {
        this.re = re;
        this.im = im;
    }

    // Propiedad para obtener la parte real del número complejo.
    real getReal() const pure, nothrow {
        return re;
    }

    // Propiedad para obtener la parte imaginaria del número complejo.
    real getImaginary() const pure, nothrow {
        return im;
    }

    // Operador unario que devuelve el negativo del número complejo.
    -this() pure, nothrow {
        return new complex(-re, -im);
    }

    // Operador binario que devuelve la suma de dos números complejos.
    complex +this(complex other) pure, nothrow {
        return new complex(re + other.re, im + other.im);
    }

    // Operador binario que devuelve la resta de dos números complejos.
    complex -this(complex other) pure, nothrow {
        return new complex(re - other.re, im - other.im);
    }

    // Operador binario que devuelve el producto de dos números complejos.
    complex *this(complex other) pure, nothrow {
        return new complex(re * other.re - im * other.im, re * other.im + im * other.re);
    }

    // Operador binario que devuelve el cociente de dos números complejos.
    complex /this(complex other) pure, nothrow {
        if (other.re == 0 && other.im == 0) {
            throw new Exception("División por cero");
        }
        real denominator = other.re * other.re + other.im * other.im;
        return new complex((re * other.re + im * other.im) / denominator, (im * other.re - re * other.im) / denominator);
    }

    // Operador de asignación que asigna el valor de otro número complejo a este número complejo.
    this operator=(complex other) pure, nothrow {
        re = other.re;
        im = other.im;
        return this;
    }

    // Función que devuelve una cadena con la representación textual del número complejo.
    string toString() const pure, nothrow {
        string result = re.toString();
        if (im >= 0) {
            result += " + ";
        } else {
            result += " - ";
        }
        result += im.abs().toString();
        result += "i";
        return result;
    }
}

// Función principal que crea dos números complejos, los suma y muestra el resultado.
void main() {
    complex a = new complex(3, 4);
    complex b = new complex(5, -2);
    complex c = a + b;

    writeln(a, " + ", b, " = ", c);
}
```

**Explicación del código:**

* El código define una clase `complex` para representar números complejos.
* La clase tiene dos atributos privados, `re` y `im`, que representan la parte real e imaginaria del número complejo, respectivamente.
* La clase tiene un constructor por defecto que inicializa el número complejo a 0 + 0i.
* La clase también tiene un constructor que inicializa el número complejo con la parte real y imaginaria dadas.
* La clase tiene propiedades para obtener la parte real y la parte imaginaria del número complejo.
* La clase tiene operadores unarios y binarios que permiten realizar operaciones aritméticas con números complejos.
* La clase tiene un operador de asignación que permite asignar el valor de otro número complejo a este número complejo.
* La clase tiene una función `toString()` que devuelve una cadena con la representación textual del número complejo.
* La función `main()` crea dos números complejos, los suma y muestra el resultado.