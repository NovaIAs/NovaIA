Certamente! Aqui está um código complexo em FORTRAN que realizará a resolução de um sistema de equações lineares utilizando o método de Eliminação de Gauss-Jordan:

```fortran
PROGRAM ResolucaoSistemaLinear
    IMPLICIT NONE
    
    INTEGER :: N, I, J, K
    REAL, DIMENSION(:,:), ALLOCATABLE :: A, B, X
    
    ! Entrada dos dados
    WRITE(*,*) "Digite o número de equações do sistema:"
    READ(*,*) N
    
    ! Alocação dinâmica dos arrays
    ALLOCATE(A(N,N), B(N,1), X(N,1))
    
    ! Entrada dos coeficientes das equações
    WRITE(*,*) "Digite os coeficientes das equações:"
    DO I = 1, N
        WRITE(*,*) "Equação ", I
        DO J = 1, N
            WRITE(*,*) "Coeficiente ", J
            READ(*,*) A(I,J)
        END DO
    END DO
    
    ! Entrada dos termos independentes das equações
    WRITE(*,*) "Digite os termos independentes das equações:"
    DO I = 1, N
        WRITE(*,*) "Termo independente ", I
        READ(*,*) B(I,1)
    END DO
    
    ! Eliminação de Gauss-Jordan
    DO K = 1, N
        DO I = 1, N
            IF (I /= K) THEN
                A(I,:) = A(I,:) - A(I,K) / A(K,K) * A(K,:)
                B(I,1) = B(I,1) - A(I,K) / A(K,K) * B(K,1)
            END IF
        END DO
        A(K,:) = A(K,:) / A(K,K)
        B(K,1) = B(K,1) / A(K,K)
    END DO
    
    ! Solução do sistema
    X = B
    
    ! Saída dos resultados
    WRITE(*,*) "A solução do sistema é:"
    DO I = 1, N
        WRITE(*,*) "X(", I, ") = ", X(I,1)
    END DO
    
    ! Liberação da memória alocada
    DEALLOCATE(A, B, X)
    
END PROGRAM ResolucaoSistemaLinear
```

Este código em FORTRAN resolve um sistema de equações lineares representado por uma matriz de coeficientes `A` e um vetor de termos independentes `B`. Ele utiliza o método de Eliminação de Gauss-Jordan para transformar a matriz `A` em uma matriz identidade e encontrar a solução do sistema.

O programa começa solicitando ao usuário o número de equações do sistema. Em seguida, ele cria matrizes `A` e `B` de acordo com o número de equações fornecido. O usuário é solicitado a inserir os coeficientes das equações na matriz `A` e os termos independentes na matriz `B`.

O método de Eliminação de Gauss-Jordan é então aplicado, onde as linhas da matriz `A` são manipuladas para transformar `A` em uma matriz identidade e, simultaneamente, os elementos correspondentes de `B` são atualizados. Finalmente, a solução do sistema é armazenada na matriz `X` e é exibida na tela.

Espero que este código atenda às suas expectativas!