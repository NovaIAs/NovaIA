Claro! Aqui está um exemplo de código complexo em FORTRAN que realiza o cálculo do fatorial de um número:

```fortran
PROGRAM Fatorial
  IMPLICIT NONE

  INTEGER :: num, i
  INTEGER, DIMENSION(:), ALLOCATABLE :: fatorial

  ! Solicita o número para calcular o fatorial
  WRITE(*,*) 'Digite um número inteiro positivo:'
  READ(*,*) num

  ! Verifica se o número é válido
  IF (num <= 0) THEN
    WRITE(*,*) 'Erro: o número deve ser positivo!'
    STOP
  END IF

  ! Aloca memória para o vetor que armazenará os fatoriais
  ALLOCATE(fatorial(num))

  ! Calcula o fatorial de cada número até o valor informado
  DO i = 1, num
    fatorial(i) = FatorialRecursivo(i)
  END DO

  ! Exibe os resultados
  WRITE(*,*) 'Os fatoriais de 1 a', num, ' são:'
  DO i = 1, num
    WRITE(*,*) i, '! =', fatorial(i)
  END DO

  ! Libera a memória alocada
  DEALLOCATE(fatorial)

CONTAINS

  RECURSIVE FUNCTION FatorialRecursivo(n) RESULT(result)
    INTEGER, INTENT(IN) :: n
    INTEGER :: result

    IF (n == 0) THEN
      result = 1
    ELSE
      result = n * FatorialRecursivo(n - 1)
    END IF
  END FUNCTION FatorialRecursivo

END PROGRAM Fatorial
```

Este código em FORTRAN calcula o fatorial de um número inteiro positivo fornecido pelo usuário. Ele utiliza uma função recursiva chamada `FatorialRecursivo` para realizar o cálculo. No programa principal, é solicitado ao usuário que digite um número, e em seguida é verificado se o número é válido (ou seja, se é maior que zero). Alocamos um vetor chamado `fatorial` com tamanho igual ao número fornecido pelo usuário.

Em seguida, utilizamos um loop DO para calcular o fatorial de cada número de 1 a `num` utilizando a função recursiva `FatorialRecursivo`. Armazenamos os resultados no vetor `fatorial`.

Por fim, exibimos os resultados na tela utilizando outro loop DO. O programa finaliza liberando a memória alocada pelo vetor `fatorial` utilizando a função DEALLOCATE.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, por favor, me avise.