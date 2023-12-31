Claro! Aqui está um exemplo de código complexo em FORTRAN, escrito em português e com uma explicação detalhada:

```fortran
program calculadora_complexa

    implicit none
    
    integer :: n, i, j, k
    real :: matriz_a(100, 100), matriz_b(100, 100), matriz_c(100, 100)
    real :: soma, media, produto, maximo, minimo
    
    ! Entrada do usuário para o tamanho das matrizes
    write(*,*) "Digite o tamanho das matrizes quadradas (n x n):"
    read(*,*) n
    
    ! Preenchimento das matrizes A e B com valores aleatórios entre 0 e 1
    call preencher_matriz(matriz_a, n)
    call preencher_matriz(matriz_b, n)
    
    ! Cálculo da soma das matrizes A e B
    do i = 1, n
        do j = 1, n
            matriz_c(i, j) = matriz_a(i, j) + matriz_b(i, j)
        end do
    end do
    
    ! Cálculo da média dos valores da matriz C
    soma = 0.0
    do i = 1, n
        do j = 1, n
            soma = soma + matriz_c(i, j)
        end do
    end do
    media = soma / (n * n)
    
    ! Cálculo do produto das matrizes A e B
    do i = 1, n
        do j = 1, n
            produto = 0.0
            do k = 1, n
                produto = produto + matriz_a(i, k) * matriz_b(k, j)
            end do
            matriz_c(i, j) = produto
        end do
    end do
    
    ! Cálculo do valor máximo e mínimo da matriz C
    maximo = matriz_c(1, 1)
    minimo = matriz_c(1, 1)
    do i = 1, n
        do j = 1, n
            if (matriz_c(i, j) > maximo) then
                maximo = matriz_c(i, j)
            end if
            if (matriz_c(i, j) < minimo) then
                minimo = matriz_c(i, j)
            end if
        end do
    end do
    
    ! Impressão dos resultados
    write(*,*) "A soma das matrizes A e B é:"
    do i = 1, n
        do j = 1, n
            write(*,'(F6.2)',advance='no') matriz_c(i, j)
        end do
        write(*,*)
    end do
    
    write(*,*)
    write(*,*) "A média dos valores da matriz C é:", media
    
    write(*,*)
    write(*,*) "O produto das matrizes A e B é:"
    do i = 1, n
        do j = 1, n
            write(*,'(F6.2)',advance='no') matriz_c(i, j)
        end do
        write(*,*)
    end do
    
    write(*,*)
    write(*,*) "O valor máximo da matriz C é:", maximo
    write(*,*) "O valor mínimo da matriz C é:", minimo
    
contains

    ! Subrotina para preencher uma matriz com valores aleatórios entre 0 e 1
    subroutine preencher_matriz(matriz, n)
        implicit none
        integer, intent(in) :: n
        real, intent(out) :: matriz(n, n)
        integer :: i, j
        real :: valor
        
        do i = 1, n
            do j = 1, n
                call random_number(valor)
                matriz(i, j) = valor
            end do
        end do
    end subroutine preencher_matriz

end program calculadora_complexa
```

Este código em FORTRAN cria um programa chamado "calculadora_complexa" que realiza diversas operações com matrizes. Aqui está uma explicação detalhada do código:

1. Primeiro, declaramos as variáveis necessárias, incluindo as matrizes A, B e C, bem como variáveis auxiliares para cálculos intermediários.

2. O programa solicita ao usuário que digite o tamanho das matrizes quadradas (n x n).

3. Em seguida, usamos uma subrotina chamada "preencher_matriz" para preencher as matrizes A e B com valores aleatórios entre 0 e 1.

4. Realizamos a soma das matrizes A e B, armazenando o resultado na matriz C.

5. Calculamos a média dos valores da matriz C, somando todos os elementos e dividindo pelo número total de elementos.

6. Multiplicamos as matrizes A e B usando três loops aninhados para percorrer as linhas e colunas das matrizes.

7. Encontramos o valor máximo e mínimo da matriz C usando loops aninhados e condicionais.

8. Por fim, imprimimos os resultados: a soma das matrizes A e B, a média dos valores da matriz C, o produto das matrizes A e B e os valores máximo e mínimo da matriz C.

Espero que este exemplo atenda às suas expectativas! Se você tiver mais alguma solicitação, estou aqui para ajudar.