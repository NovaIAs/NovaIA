```fortran
program CalculoPi
  implicit none

  integer :: n, i
  real :: pi, x, y, r

  print *, "Introduce el número de puntos a utilizar:"
  read *, n

  pi = 0.0
  do i = 1, n
    x = random_number()
    y = random_number()
    r = sqrt(x**2 + y**2)
    if (r <= 1.0) then
      pi = pi + 4.0 / n
    end if
  end do

  print *, "El valor aproximado de pi es:", pi

end program CalculoPi
```

Este código calcula el valor aproximado de π mediante el método de Monte Carlo. El método de Monte Carlo es un método numérico que consiste en generar números aleatorios y utilizarlos para simular un proceso físico o matemático. En este caso, el proceso físico que se simula es el lanzamiento de dardos a un círculo. El círculo tiene un radio de 1 y está inscrito en un cuadrado de lado 2. El número de dardos que caen dentro del círculo es aproximadamente proporcional al área del círculo, que es π.

El código primero lee el número de dardos a utilizar. Luego, genera n números aleatorios entre 0 y 1, que representan las coordenadas x e y de los dardos. Calcula la distancia r de cada dardo al centro del círculo. Si r es menor o igual a 1, el dardo ha caído dentro del círculo. El código cuenta el número de dardos que han caído dentro del círculo y lo divide por el número total de dardos para obtener una estimación de π.

El código también imprime el valor aproximado de π en la consola.