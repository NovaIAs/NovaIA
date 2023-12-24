Claro! Aqui está um código complexo em FORTRAN que realiza a multiplicação de matrizes. O código está comentado em português para facilitar a compreensão:

```fortran
program multiplicacao_matrizes
    implicit none
    integer :: i, j, k, n, m, p
    real, allocatable :: matriz1(:,:), matriz2(:,:), resultado(:,:)

    ! Tamanho das matrizes
    n = 3
    m = 4
    p = 2

    ! Alocar memória para as matrizes
    allocate(matriz1(n,m))
    allocate(matriz2(m,p))
    allocate(resultado(n,p))

    ! Preencher as matrizes com dados de exemplo
    matriz1 = reshape((/1,2,3,4,5,6,7,8,9,10,11,12/), [n,m])
    matriz2 = reshape((/1,0,0,1,1,0/), [m,p])

    ! Realizar a multiplicação de matrizes
    do i = 1, n
        do j = 1, p
            resultado(i,j) = 0.0
            do k = 1, m
                resultado(i,j) = resultado(i,j) + matriz1(i,k) * matriz2(k,j)
            end do
        end do
    end do

    ! Imprimir a matriz resultado
    do i = 1, n
        do j = 1, p
            write(*, "(F6.2, '  ')", advance="no") resultado(i, j)
        end do
        write(*,*)
    end do

    ! Liberar memória das matrizes
    deallocate(matriz1)
    deallocate(matriz2)
    deallocate(resultado)

end program multiplicacao_matrizes
```

Neste código, começamos declarando as variáveis necessárias, incluindo as dimensões das matrizes: n, m e p. Em seguida, alocamos memória para as matrizes matriz1, matriz2 e resultado usando a diretiva `allocate`.

Depois disso, preenchemos as matrizes com dados de exemplo. No exemplo apresentado, matriz1 é uma matriz 3x4 e matriz2 é uma matriz 4x2.

Em seguida, utilizamos três loops `do` aninhados para realizar a multiplicação de matrizes. O loop externo percorre as linhas da matriz resultado, o loop intermediário percorre as colunas da matriz resultado, e o loop mais interno realiza a soma dos produtos dos elementos correspondentes nas matrizes matriz1 e matriz2.

Após a multiplicação, imprimimos a matriz resultado, usando o comando `write` para formatar a saída.

Por fim, liberamos a memória alocada para as matrizes usando a diretiva `deallocate`.

Espero que este código atenda às suas expectativas! Se você tiver mais alguma solicitação, não hesite em me perguntar.