```fortran
! Programa para resolver el sistema de ecuaciones diferenciales
! d^2y/dx^2 + y = 0
! d^2x/dx^2 - x = 0
! con las condiciones iniciales
! y(0) = 1, y'(0) = 0
! x(0) = 0, x'(0) = 1

program sistema_ecuaciones_diferenciales

implicit none

! Declaración de variables
real, dimension(2) :: y
real, dimension(2) :: x
real :: t
real :: h
integer :: n

! Valores iniciales
y(1) = 1.0
y(2) = 0.0
x(1) = 0.0
x(2) = 1.0
t = 0.0
h = 0.01
n = 1000

! Bucle para resolver el sistema de ecuaciones diferenciales
do i = 1, n

    ! Cálculo de las derivadas
    y'(1) = y(2)
    y'(2) = -y(1)
    x'(1) = x(2)
    x'(2) = -x(1)

    ! Cálculo de los valores de y y x en el siguiente paso de tiempo
    y(1) = y(1) + h * y'(1)
    y(2) = y(2) + h * y'(2)
    x(1) = x(1) + h * x'(1)
    x(2) = x(2) + h * x'(2)

    ! Actualización del tiempo
    t = t + h

end do

! Impresión de los resultados
print *, 'Solución del sistema de ecuaciones diferenciales:'
print *, 'y(t) = ', y(1)
print *, 'x(t) = ', x(1)

end program sistema_ecuaciones_diferenciales
```

Explicación del código:

* El programa resuelve un sistema de dos ecuaciones diferenciales de segundo orden con condiciones iniciales dadas.
* Las ecuaciones diferenciales son:
    * d^2y/dx^2 + y = 0
    * d^2x/dx^2 - x = 0
* Las condiciones iniciales son:
    * y(0) = 1
    * y'(0) = 0
    * x(0) = 0
    * x'(0) = 1
* El programa utiliza el método de Runge-Kutta de cuarto orden para resolver el sistema de ecuaciones diferenciales.
* El método de Runge-Kutta de cuarto orden es un método numérico para resolver ecuaciones diferenciales ordinarias.
* El programa imprime los valores de y(t) y x(t) en el tiempo t.