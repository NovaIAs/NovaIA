Claro! Aqui está um exemplo de código complexo em FORTRAN, com comentários explicativos em português:

```fortran
program CalculadoraMatricial
    implicit none
    integer, parameter :: N = 3 ! Definindo a ordem das matrizes
    integer :: i, j, k ! Variáveis de controle do loop
    real :: A(N,N), B(N,N), C(N,N) ! Matrizes de entrada e de resultado

    ! Preenchendo as matrizes A e B com valores
    do i = 1, N
        do j = 1, N
            A(i,j) = i + j
            B(i,j) = i - j
        end do
    end do

    ! Calculando a multiplicação das matrizes A e B
    do i = 1, N
        do j = 1, N
            C(i,j) = 0.0 ! Inicializando o valor da célula da matriz C
            do k = 1, N
                C(i,j) = C(i,j) + A(i,k) * B(k,j) ! Realizando a multiplicação
            end do
        end do
    end do

    ! Imprimindo a matriz C resultante
    write(*,*) "Matriz C:"
    do i = 1, N
        do j = 1, N
            write(*,"(F6.2)") C(i,j) ! Imprimindo cada célula da matriz C
        end do
        write(*,*)
    end do

end program CalculadoraMatricial
```

Neste código, temos um programa que realiza a multiplicação de duas matrizes de ordem N (definida como 3 nesse exemplo) e imprime a matriz resultante C. O código utiliza loops para preencher as matrizes A e B com valores, realiza a multiplicação e armazena o resultado na matriz C. Em seguida, as células da matriz C são impressas no console.

Espero que este código atenda às suas expectativas! Caso precise de mais alguma coisa, é só pedir.