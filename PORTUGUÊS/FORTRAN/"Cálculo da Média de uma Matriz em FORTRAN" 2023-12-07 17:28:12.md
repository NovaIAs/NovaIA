Certamente! Aqui está um código complexo em FORTRAN que incorpora várias funcionalidades diferentes. O código será explicado em detalhes para facilitar o entendimento:

```fortran
! Este programa em FORTRAN calcula a média de uma matriz e exibe o resultado.

program calcula_media
    implicit none
    
    integer, parameter :: tamanho = 5   ! Tamanho da matriz
    integer :: matriz(tamanho, tamanho) ! Declaração da matriz
    real :: media                      ! Variável para armazenar a média
    integer :: i, j                    ! Índices para iteração
    
    ! Preenche a matriz com valores de exemplo
    do i = 1, tamanho
        do j = 1, tamanho
            matriz(i, j) = i + j
        end do
    end do
    
    ! Calcula a média dos elementos da matriz
    media = calcula_media_matriz(matriz, tamanho)
    
    ! Exibe o resultado
    write(*,*) 'A média da matriz é:', media
    
contains

    function calcula_media_matriz(mat, tamanho_matriz) result(media)
        integer, intent(in) :: mat(tamanho_matriz, tamanho_matriz) ! Matriz de entrada
        integer, intent(in) :: tamanho_matriz                      ! Tamanho da matriz
        real :: media                                              ! Variável para armazenar a média
        integer :: i, j                                            ! Índices para iteração
        integer :: soma = 0                                        ! Variável para armazenar a soma dos elementos
        
        ! Calcula a soma dos elementos da matriz
        do i = 1, tamanho_matriz
            do j = 1, tamanho_matriz
                soma = soma + mat(i, j)
            end do
        end do
        
        ! Calcula a média
        media = real(soma) / real(tamanho_matriz * tamanho_matriz)
        
    end function calcula_media_matriz

end program calcula_media
```

Este programa em FORTRAN calcula a média de uma matriz 5x5 preenchida com valores de exemplo. Ele usa uma função chamada `calcula_media_matriz` para realizar o cálculo da média.

Explanação:

1. O programa começa com a declaração do tamanho da matriz (`tamanho`) e a declaração da matriz (`matriz`).
2. Em seguida, declaramos a variável `media` para armazenar a média.
3. Definimos os índices `i` e `j` para iteração.
4. A matriz é preenchida com valores de exemplo usando dois loops `do`.
5. O programa chama a função `calcula_media_matriz` passando a matriz preenchida e o tamanho como argumentos.
6. A função `calcula_media_matriz` recebe a matriz e o tamanho como parâmetros de entrada, além de declarar variáveis locais.
7. Dentro da função, a soma dos elementos da matriz é calculada usando os mesmos loops `do` e armazenada na variável `soma`.
8. A média é então calculada dividindo a soma pelo número total de elementos da matriz.
9. O valor da média é retornado ao programa principal e exibido na tela.

Espero que este código complexo em FORTRAN atenda às suas expectativas! Lembre-se de ajustar o tamanho da matriz ou fazer outras alterações se necessário.