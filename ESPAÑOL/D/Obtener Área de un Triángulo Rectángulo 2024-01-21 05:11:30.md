```d
// Programa para calcular el área de un triángulo rectángulo utilizando la fórmula (base x altura) / 2.

import std.stdio, std.math;

// Definición de la función para calcular el área del triángulo.
double areaTriangulo(double base, double altura) {
    return (base * altura) / 2;
}

// Función principal del programa.
void main() {
    // Declaración de variables.
    double base, altura, area;

    // Solicitud de datos al usuario.
    writefln("Ingrese la base del triángulo (en centímetros): ");
    base = readln().toDouble();

    writefln("Ingrese la altura del triángulo (en centímetros): ");
    altura = readln().toDouble();

    // Cálculo del área del triángulo.
    area = areaTriangulo(base, altura);

    // Impresión del resultado.
    writefln("El área del triángulo es: %f centímetros cuadrados", area);
}
```

Explicación del código:

1. `import std.stdio, std.math;`: Se importan las bibliotecas estándar de entrada/salida (`stdio`) y matemáticas (`math`).

2. `double areaTriangulo(double base, double altura)`: Se define una función llamada `areaTriangulo` que toma dos parámetros: `base` y `altura`, ambos de tipo `double`, y devuelve un valor de tipo `double`. Esta función calcula el área del triángulo rectángulo utilizando la fórmula `(base x altura) / 2`.

3. `void main()`: Se define la función principal del programa. En D, el punto de entrada del programa es siempre la función `main`.

4. `double base, altura, area;`: Se declaran las variables `base`, `altura` y `area` de tipo `double`. Estas variables se utilizarán para almacenar la entrada del usuario y el resultado del cálculo del área del triángulo.

5. `writefln("Ingrese la base del triángulo (en centímetros): ");`: Se utiliza `writefln` para solicitar al usuario que ingrese la base del triángulo en centímetros.

6. `base = readln().toDouble();`: Se utiliza `readln` para leer la entrada del usuario y se convierte a un valor de tipo `double` utilizando `toDouble`.

7. `writefln("Ingrese la altura del triángulo (en centímetros): ");`: Se utiliza `writefln` para solicitar al usuario que ingrese la altura del triángulo en centímetros.

8. `altura = readln().toDouble();`: Se utiliza `readln` para leer la entrada del usuario y se convierte a un valor de tipo `double` utilizando `toDouble`.

9. `area = areaTriangulo(base, altura);`: Se llama a la función `areaTriangulo` pasándole como argumentos los valores de `base` y `altura`, y se almacena el resultado en la variable `area`.

10. `writefln("El área del triángulo es: %f centímetros cuadrados", area);`: Se utiliza `writefln` para imprimir el resultado del cálculo del área del triángulo en centímetros cuadrados.