```fortran
! Programa para resolver el sistema de ecuaciones lineales Ax = b utilizando el método de Gauss-Seidel.

! Se pide el número de ecuaciones.
print *, "Introduzca el número de ecuaciones:"
read *, n

! Se pide la matriz A.
print *, "Introduzca la matriz A:"
do i = 1, n
    do j = 1, n
        read *, a(i, j)
    end do
end do

! Se pide el vector b.
print *, "Introduzca el vector b:"
do i = 1, n
    read *, b(i)
end do

! Se inicializa el vector x con valores cero.
do i = 1, n
    x(i) = 0.0
end do

! Se itera hasta que la diferencia entre dos iteraciones consecutivas sea menor que una tolerancia dada.
do i = 1, 1000
    ! Se calcula el residuo para cada ecuación.
    do j = 1, n
        r(j) = b(j)
        do k = 1, n
            if (k /= j) then
                r(j) = r(j) - a(j, k) * x(k)
            end if
        end do
    end do

    ! Se actualiza el vector x.
    do j = 1, n
        x(j) = x(j) + r(j) / a(j, j)
    end do

    ! Se calcula la diferencia entre dos iteraciones consecutivas.
    delta = 0.0
    do j = 1, n
        delta = delta + abs(x(j) - x_old(j))
    end do

    ! Se guarda el vector x actual para la próxima iteración.
    do j = 1, n
        x_old(j) = x(j)
    end do

    ! Se comprueba si la diferencia es menor que la tolerancia dada.
    if (delta < 1.0E-6) then
        exit
    end if
end do

! Se imprime la solución.
print *, "La solución es:"
do i = 1, n
    print *, x(i)
end do
```

Explicación del código:

* El programa comienza pidiendo al usuario el número de ecuaciones que tiene el sistema.
* A continuación, se solicita la matriz A y el vector b.
* El vector x se inicializa con valores cero.
* El programa itera hasta que la diferencia entre dos iteraciones consecutivas sea menor que una tolerancia dada.
* En cada iteración, se calcula el residuo para cada ecuación, se actualiza el vector x y se calcula la diferencia entre dos iteraciones consecutivas.
* Si la diferencia es menor que la tolerancia dada, el programa sale del bucle.
* Al final, se imprime la solución del sistema de ecuaciones.