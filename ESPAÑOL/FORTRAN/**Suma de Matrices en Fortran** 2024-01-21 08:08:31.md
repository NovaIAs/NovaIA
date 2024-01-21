```fortran
PROGRAMA PRINCIPAL

! Declaración de variables
INTEGER, PARAMETER :: N = 100  ! Tamaño de los arreglos

REAL, DIMENSION(N,N) :: A, B, C  ! Arreglos para las matrices

! Inicialización de los arreglos
DO I = 1, N
    DO J = 1, N
        A(I,J) = 0.0
        B(I,J) = 0.0
        C(I,J) = 0.0
    END DO
END DO

! Llenado de los arreglos A y B con valores aleatorios
DO I = 1, N
    DO J = 1, N
        A(I,J) = RANDOM_NUMBER()
        B(I,J) = RANDOM_NUMBER()
    END DO
END DO

! Cálculo de la suma de las matrices A y B
DO I = 1, N
    DO J = 1, N
        C(I,J) = A(I,J) + B(I,J)
    END DO
END DO

! Impresión de la matriz C
DO I = 1, N
    DO J = 1, N
        WRITE(*,*) C(I,J)
    END DO
END DO

END PROGRAMA PRINCIPAL
```

Explicación del código:

- Declaración de variables: Se declaran las variables necesarias para el programa, incluyendo el tamaño de los arreglos, los propios arreglos y los parámetros necesarios para el cálculo.

- Inicialización de los arreglos: Se inicializan los arreglos A, B y C con valores cero.

- Llenado de los arreglos A y B con valores aleatorios: Se utilizan la función `RANDOM_NUMBER()` para llenar los arreglos A y B con valores aleatorios.

- Cálculo de la suma de las matrices A y B: Se realiza el cálculo de la suma de las matrices A y B, almacenando el resultado en el arreglo C.

- Impresión de la matriz C: Se imprime la matriz C, mostrando los valores de cada elemento.