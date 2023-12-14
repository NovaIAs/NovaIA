Certamente! Aqui está um código complexo em FORTRAN, escrito em português, com uma explicação detalhada do código:

```fortran
PROGRAM Calculo_Matricial
  IMPLICIT NONE

  INTEGER :: N, i, j, k
  REAL :: A(100, 100), B(100, 100), C(100, 100)

  ! Leitura do tamanho da matriz
  WRITE(*,*) 'Digite o tamanho da matriz quadrada (N x N):'
  READ(*,*) N

  ! Leitura dos elementos da matriz A
  WRITE(*,*) 'Digite os elementos da matriz A:'
  DO i = 1, N
    DO j = 1, N
      READ(*,*) A(i, j)
    END DO
  END DO

  ! Leitura dos elementos da matriz B
  WRITE(*,*) 'Digite os elementos da matriz B:'
  DO i = 1, N
    DO j = 1, N
      READ(*,*) B(i, j)
    END DO
  END DO

  ! Cálculo da multiplicação das matrizes A e B
  DO i = 1, N
    DO j = 1, N
      C(i, j) = 0.0
      DO k = 1, N
        C(i, j) = C(i, j) + A(i, k) * B(k, j)
      END DO
    END DO
  END DO

  ! Impressão da matriz resultante C
  WRITE(*,*) 'A matriz resultante C é:'
  DO i = 1, N
    DO j = 1, N
      WRITE(*,*) C(i, j)
    END DO
  END DO

END PROGRAM Calculo_Matricial
```

Explicação do código:

1. A declaração `IMPLICIT NONE` é usada para garantir que todas as variáveis sejam declaradas explicitamente.

2. A variável `N` é usada para armazenar o tamanho da matriz quadrada.

3. As matrizes `A`, `B` e `C` são declaradas como matrizes de tamanho máximo 100x100 para armazenar os elementos das matrizes de entrada e saída.

4. O usuário é solicitado a digitar o tamanho da matriz quadrada `N`.

5. O usuário é solicitado a digitar os elementos da matriz `A` usando loops `DO` aninhados.

6. O usuário é solicitado a digitar os elementos da matriz `B` usando loops `DO` aninhados.

7. O cálculo da multiplicação das matrizes `A` e `B` é realizado usando loops `DO` aninhados.

8. A matriz resultante `C` é impressa usando loops `DO` aninhados.

Espero que isso atenda às suas expectativas! Se você tiver mais alguma solicitação, não hesite em perguntar.