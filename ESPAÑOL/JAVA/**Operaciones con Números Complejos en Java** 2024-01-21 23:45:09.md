```java
import java.util.Scanner;

class Complejo {
    private double real;
    private double imaginario;

    public Complejo() {
        this.real = 0;
        this.imaginario = 0;
    }

    public Complejo(double real, double imaginario) {
        this.real = real;
        this.imaginario = imaginario;
    }

    public double getReal() {
        return real;
    }

    public void setReal(double real) {
        this.real = real;
    }

    public double getImaginario() {
        return imaginario;
    }

    public void setImaginario(double imaginario) {
        this.imaginario = imaginario;
    }

    public Complejo suma(Complejo otro) {
        double real = this.real + otro.real;
        double imaginario = this.imaginario + otro.imaginario;
        return new Complejo(real, imaginario);
    }

    public Complejo resta(Complejo otro) {
        double real = this.real - otro.real;
        double imaginario = this.imaginario - otro.imaginario;
        return new Complejo(real, imaginario);
    }

    public Complejo multiplicacion(Complejo otro) {
        double real = this.real * otro.real - this.imaginario * otro.imaginario;
        double imaginario = this.real * otro.imaginario + this.imaginario * otro.real;
        return new Complejo(real, imaginario);
    }

    public Complejo division(Complejo otro) {
        double denominador = otro.real * otro.real + otro.imaginario * otro.imaginario;
        double real = (this.real * otro.real + this.imaginario * otro.imaginario) / denominador;
        double imaginario = (this.imaginario * otro.real - this.real * otro.imaginario) / denominador;
        return new Complejo(real, imaginario);
    }

    public String toString() {
        return "(" + this.real + ", " + this.imaginario + "i)";
    }

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);

        // Leer dos números complejos
        System.out.println("Introduce la parte real del primer número complejo:");
        double real1 = scanner.nextDouble();
        System.out.println("Introduce la parte imaginaria del primer número complejo:");
        double imaginario1 = scanner.nextDouble();
        Complejo complejo1 = new Complejo(real1, imaginario1);

        System.out.println("Introduce la parte real del segundo número complejo:");
        double real2 = scanner.nextDouble();
        System.out.println("Introduce la parte imaginaria del segundo número complejo:");
        double imaginario2 = scanner.nextDouble();
        Complejo complejo2 = new Complejo(real2, imaginario2);

        // Mostrar los números complejos
        System.out.println("Primer número complejo:");
        System.out.println(complejo1);

        System.out.println("Segundo número complejo:");
        System.out.println(complejo2);

        // Sumar los números complejos
        Complejo suma = complejo1.suma(complejo2);

        // Restar los números complejos
        Complejo resta = complejo1.resta(complejo2);

        // Multiplicar los números complejos
        Complejo multiplicacion = complejo1.multiplicacion(complejo2);

        // Dividir los números complejos
        Complejo division = complejo1.division(complejo2);

        // Mostrar los resultados
        System.out.println("Suma:");
        System.out.println(suma);

        System.out.println("Resta:");
        System.out.println(resta);

        System.out.println("Multiplicación:");
        System.out.println(multiplicacion);

        System.out.println("División:");
        System.out.println(division);
    }
}
```

Explicación del código:

* Las dos primeras líneas importan la clase `Scanner` y la clase `Complejo`.

* La clase `Complejo` es una clase que representa un número complejo. Tiene dos propiedades, `real` e `imaginario`, que representan la parte real y la parte imaginaria del número complejo, respectivamente.

* La clase `Complejo` tiene un constructor sin parámetros que crea un número complejo con la parte real y la parte imaginaria iguales a cero. También tiene un constructor con dos parámetros que crea un número complejo con la parte real y la parte imaginaria especificadas.

* La clase `Complejo` tiene cuatro métodos: `suma()`, `resta()`, `multiplicacion()` y `division()`, que devuelven el resultado de sumar, restar, multiplicar y dividir dos números complejos, respectivamente.

* La clase `Complejo` también tiene un método `toString()` que devuelve una cadena de caracteres que representa al número complejo.

* El método `main()` es el punto de entrada del programa.

* El método `main()` utiliza la clase `Scanner` para leer dos números complejos del usuario.

* El método `main()` llama a los métodos `suma()`, `resta()`, `multiplicacion()` y `division()` para calcular la suma, la resta, la multiplicación y la división de los dos números complejos, respectivamente.

* El método `main()` muestra los resultados de las operaciones en la consola.