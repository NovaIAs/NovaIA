**Tarea**: Desarrollar un programa en PORTUGOL que simule una calculadora científica, con operaciones básicas como suma, resta, multiplicación, división y exponenciación, así como también funciones trigonométricas y logarítmicas. Debe tener una interfaz de usuario sencilla y permitir al usuario introducir los valores y las operaciones deseadas.

**Código**:

```portuGOL
programa Calculadora {

    procedimiento principal() {
        // Declaración de variables
        real valor1, valor2, resultado;
        char operador;

        // Bucle principal
        mientras (verdadero) {
            // Mostrar el menú
            escribir("Menú de la calculadora:")
            escribir("1. Suma")
            escribir("2. Resta")
            escribir("3. Multiplicación")
            escribir("4. División")
            escribir("5. Exponenciación")
            escribir("6. Seno")
            escribir("7. Coseno")
            escribir("8. Tangente")
            escribir("9. Logaritmo")
            escribir("10. Salir")

            // Leer la opción del usuario
            leer(op)

            // Realizar la operación correspondiente
            switch (op) {
                caso 1:
                    escribir("Introduce el primer valor:")
                    leer(valor1)
                    escribir("Introduce el segundo valor:")
                    leer(valor2)
                    resultado = valor1 + valor2
                    escribir("El resultado es:", resultado)
                    break

                caso 2:
                    escribir("Introduce el primer valor:")
                    leer(valor1)
                    escribir("Introduce el segundo valor:")
                    leer(valor2)
                    resultado = valor1 - valor2
                    escribir("El resultado es:", resultado)
                    break

                caso 3:
                    escribir("Introduce el primer valor:")
                    leer(valor1)
                    escribir("Introduce el segundo valor:")
                    leer(valor2)
                    resultado = valor1 * valor2
                    escribir("El resultado es:", resultado)
                    break

                caso 4:
                    escribir("Introduce el primer valor:")
                    leer(valor1)
                    escribir("Introduce el segundo valor:")
                    leer(valor2)
                    si (valor2 == 0) {
                        escribir("Error: no se puede dividir entre 0")
                    } sino {
                        resultado = valor1 / valor2
                        escribir("El resultado es:", resultado)
                    }
                    break

                caso 5:
                    escribir("Introduce la base:")
                    leer(valor1)
                    escribir("Introduce el exponente:")
                    leer(valor2)
                    resultado = potencia(valor1, valor2)
                    escribir("El resultado es:", resultado)
                    break

                caso 6:
                    escribir("Introduce el ángulo en radianes:")
                    leer(valor1)
                    resultado = seno(valor1)
                    escribir("El resultado es:", resultado)
                    break

                caso 7:
                    escribir("Introduce el ángulo en radianes:")
                    leer(valor1)
                    resultado = coseno(valor1)
                    escribir("El resultado es:", resultado)
                    break

                caso 8:
                    escribir("Introduce el ángulo en radianes:")
                    leer(valor1)
                    resultado = tangente(valor1)
                    escribir("El resultado es:", resultado)
                    break

                caso 9:
                    escribir("Introduce el número:")
                    leer(valor1)
                    resultado = logaritmo(valor1)
                    escribir("El resultado es:", resultado)
                    break

                caso 10:
                    escribir("Saliendo de la calculadora")
                    terminar()
                    break

                default:
                    escribir("Opción no válida")
            }
        }
    }

    // Funciones matemáticas

    real potencia(real base, real exponente) {
        real resultado = 1

        si (exponente == 0) {
            devolver resultado
        } sino si (exponente < 0) {
            base = 1 / base
            exponente = -exponente
        }

        mientras (exponente > 0) {
            si (exponente % 2 == 0) {
                base = base * base
                exponente = exponente / 2
            } sino {
                resultado = resultado * base
                exponente = exponente - 1
            }
        }

        devolver resultado
    }

    real seno(real ángulo) {
        real resultado = 0
        real término = ángulo
        real denominador = 1

        mientras (abs(término) > 0.0001) {
            resultado = resultado + término
            denominador = denominador * 2 * 3
            término = -término * ángulo * ángulo / denominador
        }

        devolver resultado
    }

    real coseno(real ángulo) {
        real resultado = 1
        real término = 1
        real denominador = 1

        mientras (abs(término) > 0.0001) {
            resultado = resultado + término
            denominador = denominador * 2 * 3
            término = -término * ángulo * ángulo / denominador
        }

        devolver resultado
    }

    real tangente(real ángulo) {
        real seno_ángulo = seno(ángulo)
        real coseno_ángulo = coseno(ángulo)

        si (coseno_ángulo == 0) {
            devolver infinito()
        } sino {
            devolver seno_ángulo / coseno_ángulo
        }
    }

    real logaritmo(real número) {
        real resultado = 0
        real término = 1
        real denominador = 1

        mientras (término > 0.0001) {
            resultado = resultado + término
            término = término * (número - 1) / denominador
            denominador = denominador + 1
        }

        devolver resultado
    }

}

```

**Explicación del código**:

* El código comienza con la declaración del programa `Calculadora` y el procedimiento principal `principal()`, que es el punto de entrada del programa.
* El procedimiento principal contiene un bucle `mientras` que se ejecuta hasta que el usuario introduce la opción 10 para salir de la calculadora.
* Dentro del bucle, el programa muestra el menú de opciones y lee la opción introducida por el usuario.
* Según la opción elegida, el programa realiza la operación correspondiente.
* Para realizar las operaciones matemáticas, el programa utiliza una serie de funciones definidas al final del código.
* Las funciones matemáticas utilizan un algoritmo iterativo para calcular el resultado de la operación.
* El programa también incluye una función para calcular el logaritmo de un número utilizando la serie de Taylor.
* El programa termina cuando el usuario introduce la opción 10 para salir de la calculadora.