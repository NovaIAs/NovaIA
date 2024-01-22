```cool
programa Calculadora {

    seccion Constantes {

        numero Uno: Int = 1;
        numero Dos: Int = 2;
        numero Tres: Int = 3;

        operador Suma: Char = '+';
        operador Resta: Char = '-';
        operador Multiplicacion: Char = '*';
        operador Division: Char = '/';

    }

    seccion Funciones {

        operacion(primerNumero: Int, segundoNumero: Int, operador: Char): Int {

            caso (operador) {

                Suma: {

                    return primerNumero + segundoNumero;

                }

                Resta: {

                    return primerNumero - segundoNumero;

                }

                Multiplicacion: {

                    return primerNumero * segundoNumero;

                }

                Division: {

                    return primerNumero / segundoNumero;

                }

            }

        }

        mostrarResultado(resultado: Int) {

            println(resultado);

        }

    }

    seccion Principal {

        var primerNumero: Int = Uno;
        var segundoNumero: Int = Dos;
        var operador: Char = Suma;

        var resultado: Int = operacion(primerNumero, segundoNumero, operador);

        mostrarResultado(resultado);

    }

}
```

Este código es una calculadora simple que realiza operaciones básicas de suma, resta, multiplicación y división.

El código está dividido en tres secciones:

* **Constantes**: Esta sección define las constantes que se utilizan en el programa, como los números y los operadores.
* **Funciones**: Esta sección define las funciones que se utilizan en el programa, como la función `operacion()` que realiza la operación matemática especificada.
* **Principal**: Esta sección es el punto de entrada del programa. Aquí se crean las variables, se llaman a las funciones y se muestra el resultado al usuario.

El código utiliza la instrucción `caso` para elegir la operación matemática que se debe realizar. La instrucción `caso` compara el valor de la variable `operador` con los valores de los casos definidos y ejecuta el código del caso correspondiente.

El código también utiliza la instrucción `println()` para mostrar el resultado de la operación al usuario. La instrucción `println()` toma una expresión como argumento y la muestra en la consola.

Este código es un ejemplo de cómo se puede utilizar el lenguaje COOL para crear programas complejos.