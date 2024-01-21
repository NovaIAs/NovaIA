```fortran

! Programa principal

program ejemplo

! Declaración de variables

integer :: i, j, k
real :: a(10, 10), b(10, 10), c(10, 10)

! Lectura de datos

open(10, file='a.dat')
do i = 1, 10
  do j = 1, 10
    read(10, *) a(i, j)
  end do
end do
close(10)

open(20, file='b.dat')
do i = 1, 10
  do j = 1, 10
    read(20, *) b(i, j)
  end do
end do
close(20)

! Cálculo de la matriz producto

do i = 1, 10
  do j = 1, 10
    c(i, j) = 0
    do k = 1, 10
      c(i, j) = c(i, j) + a(i, k) * b(k, j)
    end do
  end do
end do

! Escritura de resultados

open(30, file='c.dat')
do i = 1, 10
  do j = 1, 10
    write(30, *) c(i, j)
  end do
end do
close(30)

end program ejemplo

```

Explicación del código:

* El programa principal es `ejemplo`.
* Se declaran las variables necesarias: `i`, `j` y `k` son enteros, `a`, `b` y `c` son matrices de 10x10.
* Se leen los datos de los archivos `a.dat` y `b.dat`.
* Se calcula la matriz producto `c` multiplicando las matrices `a` y `b`.
* Se escriben los resultados en el archivo `c.dat`.