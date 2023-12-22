Claro! Aqui está um código complexo em FORTRAN que utiliza matrizes e realiza operações matemáticas avançadas:

```fortran
PROGRAM CalculadoraMatricial
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: N = 3 ! Tamanho da matriz
  
  INTEGER :: i, j, k ! Variáveis auxiliares
  REAL :: A(N,N), B(N,N), C(N,N) ! Matrizes
  
  ! Preenche as matrizes A e B com valores aleatórios entre 0 e 1
  CALL RANDOM_NUMBER(A)
  CALL RANDOM_NUMBER(B)
  
  ! Imprime a matriz A
  WRITE(*, '(A)') 'Matriz A:'
  DO i = 1, N
    WRITE(*, '(3F8.2)') (A(i,j), j = 1, N)
  END DO
  
  ! Imprime a matriz B
  WRITE(*, '(A)') 'Matriz B:'
  DO i = 1, N
    WRITE(*, '(3F8.2)') (B(i,j), j = 1, N)
  END DO
  
  ! Realiza a multiplicação matricial: C = A * B
  DO i = 1, N
    DO j = 1, N
      C(i,j) = 0.0
      DO k = 1, N
        C(i,j) = C(i,j) + A(i,k) * B(k,j)
      END DO
    END DO
  END DO
  
  ! Imprime a matriz resultante C
  WRITE(*, '(A)') 'Matriz C = A * B:'
  DO i = 1, N
    WRITE(*, '(3F8.2)') (C(i,j), j = 1, N)
  END DO

END PROGRAM CalculadoraMatricial
```

Este código é um programa em FORTRAN que realiza a multiplicação de duas matrizes `A` e `B` de tamanho `N x N`. O valor de `N` é definido como 3.

O código utiliza um loop `DO` aninhado para calcular o produto matricial `C = A * B`. O resultado é armazenado na matriz `C`. Em seguida, o programa imprime as matrizes `A`, `B` e `C` na saída padrão.

O programa utiliza a função `RANDOM_NUMBER` para preencher as matrizes `A` e `B` com valores aleatórios entre 0 e 1. Em seguida, as matrizes são exibidas na tela antes e depois da operação de multiplicação.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.