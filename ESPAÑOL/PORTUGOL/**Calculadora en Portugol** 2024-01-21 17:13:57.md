```PORTUGOL
PROC {operación} ()
BEGIN
    VARIÁVEL: número1, número2, resultado

    // Leer los valores de los números
    ESCRIBA ("Ingrese el primer número:")
    LEIA (número1)

    ESCRIBA ("Ingrese el segundo número:")
    LEIA (número2)

    // Calcular el resultado
    SE {operación} = "suma"
    ENTONCES
        resultado = número1 + número2
    EN CASO CONTRARIO
        SE {operación} = "resta"
        ENTONCES
            resultado = número1 - número2
        EN CASO CONTRARIO
            SE {operación} = "multiplicación"
            ENTONCES
                resultado = número1 * número2
            EN CASO CONTRARIO
                SE {operación} = "división"
                ENTONCES
                    resultado = número1 / número2
                EN CASO CONTRARIO
                    ESCRIBA ("Operación no válida")
                FIN SI
            FIN SI
        FIN SI
    FIN SI

    // Mostrar el resultado
    ESCRIBA ("El resultado es:", resultado)
END

// Iniciar el programa principal
PROC {main} ()
BEGIN
    VARIÁVEL: operación

    // Mostrar el menú de opciones
    MIENTRAS VERDADERO
    HACER
        ESCRIBA ("--- Calculadora ---")
        ESCRIBA ("1. Suma")
        ESCRIBA ("2. Resta")
        ESCRIBA ("3. Multiplicación")
        ESCRIBA ("4. División")
        ESCRIBA ("5. Salir")
        ESCRIBA ("Elija una opción:")
        LEIA (operación)

        // Elegir la operación y llamar al procedimiento correspondiente
        SE operación = 1
        ENTONCES
            LLAMAR {suma} ()
        EN CASO CONTRARIO
            SE operación = 2
            ENTONCES
                LLAMAR {resta} ()
            EN CASO CONTRARIO
                SE operación = 3
                ENTONCES
                    LLAMAR {multiplicación} ()
                EN CASO CONTRARIO
                    SE operación = 4
                    ENTONCES
                        LLAMAR {división} ()
                    EN CASO CONTRARIO
                        SE operación = 5
                        ENTONCES
                            SALIR
                        FIN SI
                    FIN SI
                FIN SI
            FIN SI
        FIN SI
    FIN MIENTRAS

END

```

**Explicación del código:**

1. **Procedimiento {operación}():** Este procedimiento realiza la operación aritmética especificada por el parámetro {operación}. Recibe dos números como entrada y devuelve el resultado de la operación.
2. **Procedimiento {main}():** Este es el procedimiento principal del programa. Muestra el menú de opciones y permite al usuario elegir una operación. Luego, llama al procedimiento correspondiente para realizar la operación seleccionada.
3. **Menú de opciones:** El menú de opciones permite al usuario elegir entre las siguientes operaciones: suma, resta, multiplicación, división y salir.
4. **Selección de la operación:** El usuario ingresa el número de la operación que desea realizar.
5. **Llamada al procedimiento:** Dependiendo de la operación seleccionada, el procedimiento {main}() llama al procedimiento correspondiente para realizar la operación.
6. **Salida del programa:** Si el usuario elige la opción "Salir", el programa termina.

**Ejemplo de ejecución:**

```
--- Calculadora ---
1. Suma
2. Resta
3. Multiplicación
4. División
5. Salir
Elija una opción: 1

Ingrese el primer número: 10
Ingrese el segundo número: 5

El resultado es: 15

--- Calculadora ---
1. Suma
2. Resta
3. Multiplicación
4. División
5. Salir
Elija una opción: 2

Ingrese el primer número: 10
Ingrese el segundo número: 5

El resultado es: 5

--- Calculadora ---
1. Suma
2. Resta
3. Multiplicación
4. División
5. Salir
Elija una opción: 5

Saliendo del programa...
```