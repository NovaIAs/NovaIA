```
/*
 * Programa: Calculadora de ecuaciones de segundo grado.
 * Autor: [Tu nombre]
 * Fecha: [Fecha actual]
 */

/*
 * Módulo: Ecuaciones.cool
 * Este módulo contiene las definiciones de las funciones necesarias para resolver ecuaciones de segundo grado.
 */

módulo Ecuaciones {

    /*
     * Función: resolver
     * Esta función resuelve una ecuación de segundo grado de la forma ax^2 + bx + c = 0.
     * Parametros:
     *  - a: El coeficiente de x^2.
     *  - b: El coeficiente de x.
     *  - c: El término independiente.
     * Retorna:
     *  - Una lista con las dos soluciones de la ecuación, o una lista vacía si no tiene soluciones reales.
     */
    función resolver(a: Número, b: Número, c: Número): Lista[Número] {
        // Calcular el discriminante.
        discriminante = b^2 - 4 * a * c;

        // Comprobar si el discriminante es negativo.
        si (discriminante < 0) {
            // No hay soluciones reales.
            retornar lista_vacía;
        }

        // Calcular las dos soluciones.
        x1 = (-b + sqrt(discriminante)) / (2 * a);
        x2 = (-b - sqrt(discriminante)) / (2 * a);

        // Retornar las dos soluciones.
        retornar [x1, x2];
    }
}

/*
 * Módulo: Main.cool
 * Este módulo contiene el código principal del programa.
 */

módulo Main {

    /*
     * Función: main
     * Esta función es el punto de entrada del programa.
     * Parametros:
     *  - Ninguno.
     * Retorna:
     *  - Ninguno.
     */
    función main(): Ninguno {

        // Obtener los coeficientes de la ecuación.
        println("Introduce los coeficientes de la ecuación:");
        a = leer_número();
        b = leer_número();
        c = leer_número();

        // Resolver la ecuación.
        soluciones = Ecuaciones.resolver(a, b, c);

        // Mostrar las soluciones.
        si (soluciones.longitud() == 0) {
            println("La ecuación no tiene soluciones reales.");
        } sino {
            println("Las soluciones de la ecuación son:");
            para (solución en soluciones) {
                println(solución);
            }
        }
    }
}
```

**Explicación del código:**

El código está dividido en dos módulos: `Ecuaciones` y `Main`.

El módulo `Ecuaciones` contiene la definición de la función `resolver`, que resuelve una ecuación de segundo grado de la forma `ax^2 + bx + c = 0`. La función recibe como parámetros los coeficientes de la ecuación y retorna una lista con las dos soluciones de la ecuación, o una lista vacía si no tiene soluciones reales.

El módulo `Main` contiene el código principal del programa. La función `main` es el punto de entrada del programa. Esta función obtiene los coeficientes de la ecuación del usuario, llama a la función `resolver` para resolver la ecuación y muestra las soluciones en la consola.

**Uso del código:**

Para usar el programa, sigue estos pasos:

1. Copia el código en un archivo `.cool`.
2. Abre el archivo en un editor de texto.
3. Compila el código con el compilador de COOL.
4. Ejecuta el programa.
5. Introduce los coeficientes de la ecuación que quieres resolver.
6. El programa mostrará las soluciones de la ecuación en la consola.