Claro! Aqui está um exemplo de código complexo em FORTRAN, explicando cada seção:

```fortran
PROGRAM calculo_fatorial
  IMPLICIT NONE

  INTEGER :: n, i
  INTEGER, DIMENSION(:), ALLOCATABLE :: fatorial

  ! Solicitando ao usuário um número inteiro positivo
  WRITE(*,*) 'Digite um número inteiro positivo:'
  READ(*,*) n

  ! Alocando espaço para o vetor fatorial
  ALLOCATE(fatorial(n))

  ! Calculando o fatorial de cada número de 1 até n
  DO i = 1, n
    fatorial(i) = calcular_fatorial(i)
  END DO

  ! Imprimindo os resultados
  WRITE(*,*) 'Os fatoriais são:'
  DO i = 1, n
    WRITE(*,*) i, '!', '=', fatorial(i)
  END DO

  ! Desalocando o vetor fatorial
  DEALLOCATE(fatorial)

CONTAINS

  FUNCTION calcular_fatorial(m)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: m
    INTEGER :: calcular_fatorial

    ! Verificando se o número é zero ou negativo
    IF (m <= 0) THEN
      calcular_fatorial = 1
      RETURN
    END IF

    ! Calculando o fatorial recursivamente
    calcular_fatorial = m * calcular_fatorial(m - 1)
  END FUNCTION calcular_fatorial

END PROGRAM calculo_fatorial
```

Neste exemplo, o programa solicita ao usuário um número inteiro positivo e calcula o fatorial de cada número de 1 até esse valor. Em seguida, os resultados são impressos na tela.

O programa utiliza uma função auxiliar chamada `calcular_fatorial` para calcular o fatorial de um número de forma recursiva. A função verifica se o número é zero ou negativo e retorna 1 nesses casos. Caso contrário, ela chama a si mesma para calcular o fatorial do número anterior e o multiplica pelo número atual.

O vetor `fatorial` é alocado dinamicamente para armazenar os resultados. Após o cálculo, ele é desalocado para liberar memória.

Espero que este código atenda às suas necessidades!