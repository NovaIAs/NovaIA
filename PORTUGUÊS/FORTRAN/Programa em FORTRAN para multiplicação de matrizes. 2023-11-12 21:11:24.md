Claro! Segue um código bastante complexo em FORTRAN:

```
PROGRAM MAIN
IMPLICIT NONE

INTEGER :: I, J, K, N, M, L
REAL :: A(100,100), B(100,100), C(100,100)

WRITE(*,*) 'Entre com o número de linhas e colunas das matrizes A, B e C:'
READ(*,*) N, M, L

IF (N.LE.0.OR.M.LE.0.OR.L.LE.0) THEN
    WRITE(*,*) 'Erro: dimensões inválidas das matrizes'
    STOP
END IF

IF (N.GT.100.OR.M.GT.100.OR.L.GT.100) THEN
    WRITE(*,*) 'Erro: dimensões das matrizes excedem o limite'
    STOP
END IF

WRITE(*,*) 'Entre com os elementos da matriz A:'
DO I = 1, N
    DO J = 1, M
        READ(*,*) A(I,J)
    END DO
END DO

WRITE(*,*) 'Entre com os elementos da matriz B:'
DO I = 1, M
    DO J = 1, L
        READ(*,*) B(I,J)
    END DO
END DO

C = 0.0
DO I = 1, N
    DO J = 1, L
        DO K = 1, M
            C(I,J) = C(I,J) + A(I,K) * B(K,J)
        END DO
    END DO
END DO

WRITE(*,*) 'A matriz resultante C é:'
DO I = 1, N
    DO J = 1, L
        WRITE(*,'(F10.2)') C(I,J)
    END DO
END DO

END PROGRAM MAIN
```

Este programa é um exemplo de multiplicação de matrizes em FORTRAN. Ele começa pedindo ao usuário que insira as dimensões das matrizes A, B e C. Em seguida, ele verifica se as dimensões são válidas e se não excedem o limite de 100.

Depois disso, o programa pede ao usuário que insira os elementos da matriz A e da matriz B. Em seguida, ele realiza a multiplicação de matrizes usando três loops DO aninhados, armazenando o resultado na matriz C.

Por fim, o programa imprime a matriz resultante C. Note que, para garantir que os números sejam formatados corretamente, o programa usa uma estrutura de formato específica na instrução WRITE.

Espero que tenha gostado!