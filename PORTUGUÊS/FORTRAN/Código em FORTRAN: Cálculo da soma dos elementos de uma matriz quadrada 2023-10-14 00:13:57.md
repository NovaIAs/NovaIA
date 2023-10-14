Certamente! Aqui está um exemplo de código em FORTRAN que realiza o cálculo da soma dos elementos de uma matriz quadrada. O código é dividido em várias etapas para facilitar o entendimento:

```fortran
PROGRAM SomaMatriz
  IMPLICIT NONE

  INTEGER, PARAMETER :: N = 5
  REAL, DIMENSION(N,N) :: matriz
  REAL :: soma
  INTEGER :: i, j

  ! Inicializando a matriz com valores aleatórios
  CALL RandomizeMatriz(matriz)

  ! Imprimindo a matriz inicial
  PRINT *, "Matriz inicial:"
  CALL ImprimirMatriz(matriz)

  ! Calculando a soma dos elementos da matriz
  soma = 0.0
  DO i = 1, N
    DO j = 1, N
      soma = soma + matriz(i,j)
    END DO
  END DO

  ! Imprimindo a soma dos elementos
  PRINT *, "A soma dos elementos da matriz é:", soma

  STOP
END PROGRAM SomaMatriz


SUBROUTINE RandomizeMatriz(matriz)
  IMPLICIT NONE

  REAL, DIMENSION(:,:) :: matriz
  INTEGER :: i, j

  CALL RANDOM_SEED()

  DO i = 1, SIZE(matriz, 1)
    DO j = 1, SIZE(matriz, 2)
      matriz(i,j) = RANDOM_NUMBER()
    END DO
  END DO

END SUBROUTINE RandomizeMatriz


SUBROUTINE ImprimirMatriz(matriz)
  IMPLICIT NONE

  REAL, DIMENSION(:,:) :: matriz
  INTEGER :: i, j

  DO i = 1, SIZE(matriz, 1)
    DO j = 1, SIZE(matriz, 2)
      PRINT *, matriz(i,j)
    END DO
  END DO

END SUBROUTINE ImprimirMatriz
```

Neste código, a matriz é representada como um array bidimensional de tamanho N x N, onde N é um parâmetro definido como 5. Primeiramente, a matriz é preenchida com valores aleatórios usando a subrotina `RandomizeMatriz`. Em seguida, a matriz inicial é impressa na tela utilizando a subrotina `ImprimirMatriz`.

Após isso, a soma dos elementos da matriz é calculada usando dois loops aninhados que percorrem todos os elementos da matriz. A cada iteração, o valor do elemento é adicionado à variável `soma`. Por fim, a soma dos elementos é impressa na tela.

Espero que este código em FORTRAN tenha atendido às suas expectativas! Se precisar de mais ajuda, é só me pedir.