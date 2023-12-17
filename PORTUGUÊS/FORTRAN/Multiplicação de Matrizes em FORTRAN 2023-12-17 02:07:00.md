Claro! Aqui está um código em FORTRAN que realiza a multiplicação de duas matrizes e imprime o resultado:

```fortran
program multiplicacao_matrizes
    implicit none
    
    integer :: i, j, k
    integer, parameter :: n = 3
    real :: matrizA(n, n), matrizB(n, n), matrizC(n, n)
    
    ! Preenche as matrizes A e B com valores aleatórios
    call preencher_matrizes(matrizA, matrizB)
    
    ! Realiza a multiplicação das matrizes A e B
    do i = 1, n
        do j = 1, n
            matrizC(i, j) = 0.0
            do k = 1, n
                matrizC(i, j) = matrizC(i, j) + matrizA(i, k) * matrizB(k, j)
            end do
        end do
    end do
    
    ! Imprime a matriz resultante
    write(*, '(A)') "Matriz C:"
    do i = 1, n
        write(*, '(3F6.2)') (matrizC(i, j), j = 1, n)
    end do
    
contains

    subroutine preencher_matrizes(matrizA, matrizB)
        implicit none
        integer :: i, j
        real :: matrizA(n, n), matrizB(n, n)
        
        do i = 1, n
            do j = 1, n
                matrizA(i, j) = real(i + j)
                matrizB(i, j) = real(i - j)
            end do
        end do
    end subroutine preencher_matrizes

end program multiplicacao_matrizes
```

Este código em FORTRAN realiza a multiplicação de duas matrizes quadradas de tamanho 3x3. Primeiro, preenchemos as matrizes A e B com valores aleatórios usando a subrotina `preencher_matrizes`, que atribui valores a cada elemento das matrizes.

Em seguida, utilizamos três loops aninhados para calcular a multiplicação das matrizes A e B e armazenar o resultado na matriz C. A cada iteração, multiplicamos os elementos correspondentes das linhas de A pelos elementos correspondentes das colunas de B e somamos os produtos para obter o valor da posição da matriz resultante.

Por fim, imprimimos a matriz resultante C usando a instrução `write`, exibindo seus valores formatados.