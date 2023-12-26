Claro! Aqui está um exemplo de um código em FORTRAN que realiza um cálculo complexo de matriz:

```fortran
PROGRAM CalculoMatriz
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: N = 5 ! Tamanho da matriz quadrada
  INTEGER :: i, j, k
  REAL :: A(N,N), B(N,N), C(N,N), D(N,N)
  
  ! Preenchendo as matrizes A e B com valores
  DO i = 1, N
    DO j = 1, N
      A(i,j) = FLOAT(i + j)
      B(i,j) = FLOAT(i - j)
    END DO
  END DO
  
  ! Calculando a matriz C
  DO i = 1, N
    DO j = 1, N
      C(i,j) = 0.0
      DO k = 1, N
        C(i,j) = C(i,j) + A(i,k) * B(k,j)
      END DO
    END DO
  END DO
  
  ! Calculando a matriz D
  DO i = 1, N
    DO j = 1, N
      D(i,j) = 0.0
      DO k = 1, N
        D(i,j) = D(i,j) + A(i,k) / B(k,j)
      END DO
    END DO
  END DO
  
  ! Imprimindo as matrizes resultantes
  WRITE(*, '(A)') 'Matriz C:'
  DO i = 1, N
    DO j = 1, N
      WRITE(*, '(F8.2)', ADVANCE='NO') C(i,j)
    END DO
    WRITE(*, *)
  END DO
  
  WRITE(*, '(A)') 'Matriz D:'
  DO i = 1, N
    DO j = 1, N
      WRITE(*, '(F8.2)', ADVANCE='NO') D(i,j)
    END DO
    WRITE(*, *)
  END DO
  
END PROGRAM CalculoMatriz
```

Este código em FORTRAN realiza o cálculo de duas matrizes, C e D, a partir de duas matrizes iniciais, A e B. As matrizes A e B são preenchidas com valores, e então a matriz C é calculada multiplicando as matrizes A e B elemento por elemento. A matriz D é calculada dividindo os elementos da matriz A pelos elementos correspondentes da matriz B. 

As matrizes resultantes C e D são impressas na tela, utilizando um formatador que mostra os valores com 2 casas decimais.

Vale ressaltar que este código é apenas um exemplo de um programa em FORTRAN e pode ser adaptado ou extendido para atender a outros requisitos ou necessidades específicas.