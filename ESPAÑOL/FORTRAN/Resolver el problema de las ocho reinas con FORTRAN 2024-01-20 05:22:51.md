```fortran
! Este programa resuelve el problema de los ocho reinas.

program ocho_reinas

implicit none

integer, parameter :: n = 8  ! El tamaño del tablero

integer, dimension(n) :: reinas  ! La posiciones de las reinas

! Subrutina para inicializar el tablero.

subroutine inicializar_tablero()

  integer :: i

  do i = 1, n
    reinas(i) = 0
  end do

end subroutine inicializar_tablero

! Subrutina para comprobar si una posición es segura.

logical function es_posicion_segura(i, j)

  integer, intent(in) :: i, j  ! Las coordenadas de la posición

  integer :: k

  es_posicion_segura = .true.

  ! Comprobar si hay una reina en la misma fila.

  do k = 1, n
    if (k /= j .and. reinas(k) == i) then
      es_posicion_segura = .false.
      exit
    end if
  end do

  ! Comprobar si hay una reina en la misma diagonal.

  k = 1
  do while (j + k <= n .and. i - k >= 1)
    if (reinas(j + k) == i - k) then
      es_posicion_segura = .false.
      exit
    end if
    k = k + 1
  end do

  k = 1
  do while (j - k >= 1 .and. i - k >= 1)
    if (reinas(j - k) == i - k) then
      es_posicion_segura = .false.
      exit
    end if
    k = k + 1
  end do

end function es_posicion_segura

! Subrutina para encontrar una solución al problema.

subroutine encontrar_solucion()

  integer :: i, j

  ! Inicializar el tablero.

  call inicializar_tablero()

  ! Iterar sobre las columnas.

  do i = 1, n

    ! Iterar sobre las filas.

    do j = 1, n

      ! Comprobar si la posición es segura.

      if (es_posicion_segura(i, j)) then

        ! Colocar la reina en la posición.

        reinas(i) = j

        ! Comprobar si se ha encontrado una solución.

        if (i == n) then

          ! Imprimir la solución.

          do k = 1, n
            write(*, *) reinas(k)
          end do

          return

        end if

        ! Recursión.

        call encontrar_solucion()

        ! Quitar la reina de la posición.

        reinas(i) = 0

      end if

    end do

  end do

end subroutine encontrar_solucion

! Programa principal.

call encontrar_solucion()

end program ocho_reinas
```

Explicación del código:

* El programa comienza con la declaración de un módulo, que contiene las siguientes subrutinas:
    * `inicializar_tablero()`: Esta subrutina inicializa el tablero, colocando todas las reinas en la posición 0.
    * `es_posicion_segura(i, j)`: Esta subrutina comprueba si una posición es segura para colocar una reina, es decir, si no hay otra reina en la misma fila, columna o diagonal.
    * `encontrar_solucion()`: Esta subrutina encuentra una solución al problema de las ocho reinas, utilizando un algoritmo de búsqueda recursiva.

* El programa principal llama a la subrutina `encontrar_solucion()`, que encuentra una solución al problema y la imprime en la salida estándar.