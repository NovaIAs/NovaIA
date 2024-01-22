```fortran
program ComplejoFortran

! Declaración de variables
integer, dimension(100) :: numeros
real :: promedio
integer :: suma, contador

! Lectura de los datos
print *, "Introduce 100 números:"
do i = 1, 100
    read *, numeros(i)
end do

! Cálculo de la suma y el promedio
suma = 0
contador = 0
do i = 1, 100
    if (numeros(i) > 0) then
        suma = suma + numeros(i)
        contador = contador + 1
    end if
end do

! Cálculo del promedio
promedio = suma / contador

! Impresión de los resultados
print *, "La suma de los números positivos es:", suma
print *, "El promedio de los números positivos es:", promedio

end program ComplejoFortran
```

Explicación del código:

* Declaración de variables:

    * `numeros`: un array de enteros de dimensión 100 para almacenar los números introducidos por el usuario.
    * `promedio`: una variable real para almacenar el promedio de los números positivos.
    * `suma`: una variable entera para almacenar la suma de los números positivos.
    * `contador`: una variable entera para almacenar el número de números positivos introducidos.

* Lectura de los datos:

    * El usuario es solicitado para introducir 100 números.
    * Los números son leídos en el array `numeros`.

* Cálculo de la suma y el promedio:

    * La suma de los números positivos es calculada utilizando un bucle `do`.
    * El contador del número de números positivos es incrementado utilizando un bucle `do`.

* Cálculo del promedio:

    * El promedio de los números positivos es calculado dividiendo la suma por el contador.

* Impresión de los resultados:

    * La suma y el promedio de los números positivos son impresos en la pantalla.