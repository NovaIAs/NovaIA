```fortran
program complejo_fortran

implicit none

parameter (n = 10)
integer, dimension(n, n) :: A, B, C
real :: alpha, beta
integer :: i, j, k

! Ingresar los valores de las matrices A y B
print *, 'Ingresar los valores de la matriz A:'
do i = 1, n
    do j = 1, n
        read *, A(i, j)
    end do
end do

print *, 'Ingresar los valores de la matriz B:'
do i = 1, n
    do j = 1, n
        read *, B(i, j)
    end do
end do

! Obtener los valores de los escalares alpha y beta
print *, 'Ingresar el valor del escalar alpha:'
read *, alpha

print *, 'Ingresar el valor del escalar beta:'
read *, beta

! Calcular la matriz C = alpha*A + beta*B
do i = 1, n
    do j = 1, n
        C(i, j) = alpha * A(i, j) + beta * B(i, j)
    end do
end do

! Imprimir la matriz C
print *, 'La matriz C es:'
do i = 1, n
    do j = 1, n
        print *, C(i, j),
    end do
    print *,
end do

end program complejo_fortran
```

Explicación del código:

1. El programa comienza con la definición de las variables y parámetros necesarios, como el tamaño de las matrices y los escalares alpha y beta.

2. A continuación, se solicita al usuario que ingrese los valores de las matrices A y B.

3. Luego, se solicita al usuario que ingrese los valores de los escalares alpha y beta.

4. A continuación, se calcula la matriz C = alpha*A + beta*B utilizando un ciclo anidado.

5. Por último, se imprime la matriz C en la consola.