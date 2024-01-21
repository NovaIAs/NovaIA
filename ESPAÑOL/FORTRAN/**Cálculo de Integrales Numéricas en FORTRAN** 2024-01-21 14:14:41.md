```fortran
program Calcular_Integral_Numerica

implicit none

real(kind=8), pointer :: f, x, dx, integral
integer :: n, i

write(*,*) "Introduzca los límites de integración (a, b):"
read(*,*) a, b
write(*,*) "Introduzca el número de subintervalos:"
read(*,*) n

dx = (b - a) / n
integral = 0.

f => subrutina_f

do i = 1, n
    x = a + (i - 0.5) * dx
    integral = integral + dx * f(x)
end do

write(*,*) "El valor de la integral es:", integral

end program Calcular_Integral_Numerica

subroutine subrutina_f(x)

implicit none

real(kind=8), intent(in) :: x
real(kind=8), intent(out) :: f

f = x**2

end subroutine subrutina_f
```

Explicación del código:

* El programa comienza declarando las variables necesarias. La variable `f` es un puntero a una función que calcula el valor de la función a integrar en un punto dado. La variable `x` es el punto en el que se evalúa la función. La variable `dx` es el tamaño del subintervalo. La variable `integral` es la integral numérica. La variable `n` es el número de subintervalos. La variable `i` es el índice del subintervalo.

* A continuación, el programa solicita al usuario que introduzca los límites de integración y el número de subintervalos.

* El programa calcula el tamaño del subintervalo y la integral numérica. La integral numérica se calcula utilizando la regla del rectángulo.

* Finalmente, el programa imprime el valor de la integral numérica.

Este código es bastante complejo para un programa de Fortran. Utiliza punteros, subrutinas y la regla del rectángulo para calcular la integral numérica. El código también es bastante general y puede utilizarse para calcular la integral numérica de cualquier función.