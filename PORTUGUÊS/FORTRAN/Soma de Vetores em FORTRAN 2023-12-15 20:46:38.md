Claro! Aqui está um exemplo de um código em FORTRAN que realiza a soma de dois vetores:

```
PROGRAM soma_vetores
    IMPLICIT NONE
    
    INTEGER :: n, i
    REAL :: vetor1(100), vetor2(100), vetor_soma(100)
    
    ! Solicita ao usuário o tamanho dos vetores
    WRITE(*,*) 'Digite o tamanho dos vetores (até 100): '
    READ(*,*) n
    
    ! Solicita ao usuário os valores do primeiro vetor
    WRITE(*,*) 'Digite os valores do primeiro vetor: '
    DO i = 1, n
       WRITE(*,*) 'Valor', i, ': '
       READ(*,*) vetor1(i)
    END DO
    
    ! Solicita ao usuário os valores do segundo vetor
    WRITE(*,*) 'Digite os valores do segundo vetor: '
    DO i = 1, n
       WRITE(*,*) 'Valor', i, ': '
       READ(*,*) vetor2(i)
    END DO
    
    ! Realiza a soma dos vetores
    DO i = 1, n
       vetor_soma(i) = vetor1(i) + vetor2(i)
    END DO
    
    ! Imprime o vetor soma
    WRITE(*,*) 'Vetor soma:'
    DO i = 1, n
       WRITE(*,*) vetor_soma(i)
    END DO
    
END PROGRAM soma_vetores
```

Neste código, o programa solicita ao usuário o tamanho dos vetores e, em seguida, os valores para cada elemento dos vetores. Em seguida, o programa realiza a soma dos elementos correspondentes dos vetores e imprime o vetor soma resultante.