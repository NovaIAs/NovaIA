```fortran

! Programa para resolver un sistema de ecuaciones lineales usando el método de Gauss-Jordan.

! Declaración de variables
integer, dimension(10, 10) :: A, B
integer :: i, j, k, n

! Leer el tamaño del sistema
print *, 'Introduzca el tamaño del sistema:'
read *, n

! Leer la matriz A y el vector B
print *, 'Introduzca los coeficientes de la matriz A:'
do i = 1, n
    do j = 1, n
        read *, A(i, j)
    end do
end do

print *, 'Introduzca los términos independientes del vector B:'
do i = 1, n
    read *, B(i)
end do

! Realizar el proceso de Gauss-Jordan
do i = 1, n
    ! Encontrar el pivote en la columna actual
    k = i
    do j = i + 1, n
        if (abs(A(j, i)) > abs(A(k, i))) then
            k = j
        end if
    end do

    ! Intercambiar las filas i y k
    if (i /= k) then
        do j = 1, n
            temp = A(i, j)
            A(i, j) = A(k, j)
            A(k, j) = temp
        end do

        temp = B(i)
        B(i) = B(k)
        B(k) = temp
    end if

    ! Dividir la fila actual por el pivote
    divisor = A(i, i)
    do j = 1, n
        A(i, j) = A(i, j) / divisor
    end do
    B(i) = B(i) / divisor

    ! Restar múltiplos de la fila actual de las demás filas
    do j = 1, n
        if (j /= i) then
            factor = A(j, i)
            do k = 1, n
                A(j, k) = A(j, k) - factor * A(i, k)
            end do
            B(j) = B(j) - factor * B(i)
        end if
    end do
end do

! Imprimir la matriz A y el vector B resultantes
print *, 'Matriz A resultante:'
do i = 1, n
    do j = 1, n
        print *, A(i, j),
    end do
    print *,
end do

print *, 'Vector B resultante:'
do i = 1, n
    print *, B(i)
end do

end program gauss_jordan

```

**Explicación del código:**

1. **Declaración de variables:**

    - `A` es una matriz de tamaño `10 x 10` para almacenar los coeficientes de la matriz A.
    - `B` es un vector de tamaño `10` para almacenar los términos independientes del vector B.
    - `i`, `j`, y `k` son variables de índice.
    - `n` es el tamaño del sistema de ecuaciones lineales.

2. **Lectura del tamaño del sistema:**

    El usuario ingresa el tamaño del sistema, que debe ser un número entero positivo.

3. **Lectura de la matriz A y el vector B:**

    El usuario ingresa los coeficientes de la matriz A y los términos independientes del vector B.

4. **Realización del proceso de Gauss-Jordan:**

    Este proceso se realiza en un bucle `do` que itera sobre las filas de la matriz A.

    - **Encontrar el pivote en la columna actual:**

        Se encuentra el índice de la fila que contiene el valor absoluto máximo en la columna actual. Este valor se almacena en la variable `k`.

    - **Intercambiar las filas i y k:**

        Si `i` y `k` son diferentes, se intercambian las filas `i` y `k` en la matriz A y en el vector B.

    - **Dividir la fila actual por el pivote:**

        La fila actual se divide por el valor del pivote para convertirlo en 1.

    - **Restar múltiplos de la fila actual de las demás filas:**

        Para cada fila `j` que no sea la fila actual, se restan múltiplos de la fila actual de la fila `j` para eliminar los coeficientes no deseados.

5. **Imprimir la matriz A y el vector B resultantes:**

    Se imprimen la matriz A y el vector B resultantes para mostrar la solución del sistema de ecuaciones lineales.