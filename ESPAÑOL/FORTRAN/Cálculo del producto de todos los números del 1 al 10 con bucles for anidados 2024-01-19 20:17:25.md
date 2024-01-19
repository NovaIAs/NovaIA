```fortran
program CalculoNumerico
implicit none
real :: x, y, z
integer :: i, j, k

! Inicialización de variables
x = 1.0
y = 2.0
z = 3.0

! Bucle for anidado para calcular el producto de todos los números del 1 al 10
do i = 1, 10
  do j = 1, 10
    do k = 1, 10
      z = z * x * y
    end do
  end do
end do

! Impresión del resultado
print *, "El resultado del producto es:", z

end program CalculoNumerico
```

Explicación del código:

* El programa `CalculoNumerico` calcula el producto de todos los números del 1 al 10 utilizando bucles for anidados.
* La variable `x` se inicializa con el valor 1.0, la variable `y` con el valor 2.0 y la variable `z` con el valor 3.0.
* El bucle for anidado recorre todos los números del 1 al 10 tres veces, y en cada iteración se multiplica el valor de `z` por el producto de `x` e `y`.
* El resultado final del producto se imprime en la pantalla.