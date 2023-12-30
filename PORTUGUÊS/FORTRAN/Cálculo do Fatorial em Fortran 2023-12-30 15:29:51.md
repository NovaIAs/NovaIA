Claro! Aqui está um exemplo de código em FORTRAN que criei para você. Este código é um pouco complexo e utiliza conceitos avançados da linguagem. Certifique-se de que está executando-o em um compilador FORTRAN compatível.

```fortran
PROGRAM calculo_fatorial
  IMPLICIT NONE

  INTEGER :: num, i
  INTEGER, DIMENSION(:), ALLOCATABLE :: fatorial

  ! Solicita ao usuário um número inteiro positivo
  WRITE(*, *) 'Digite um número inteiro positivo:'
  READ(*, *) num

  ! Aloca espaço para o vetor fatorial
  ALLOCATE(fatorial(num))

  ! Calcula o fatorial de cada número de 1 até num
  DO i = 1, num
    fatorial(i) = calcular_fatorial(i)
  END DO

  ! Imprime o vetor fatorial na tela
  WRITE(*, *) 'O fatorial dos números de 1 até ', num, ' é:'
  WRITE(*, *) fatorial

  ! Libera a memória alocada para o vetor fatorial
  DEALLOCATE(fatorial)

CONTAINS

  FUNCTION calcular_fatorial(n) RESULT(resultado)
    IMPLICIT NONE

    INTEGER, INTENT(IN) :: n
    INTEGER :: resultado

    ! Verifica se o número é igual a 0 ou 1
    IF (n == 0 .OR. n == 1) THEN
      resultado = 1
      RETURN
    END IF

    ! Calcula o fatorial de n utilizando recursão
    resultado = n * calcular_fatorial(n - 1)

  END FUNCTION calcular_fatorial

END PROGRAM calculo_fatorial
```

Neste código, criamos um programa que solicita ao usuário um número inteiro positivo e calcula o fatorial de cada número de 1 até o número informado. Utilizamos um vetor alocado dinamicamente para armazenar os resultados dos fatoriais.

A função `calcular_fatorial` é responsável por calcular o fatorial de um número dado utilizando recursão. Ela verifica se o número é igual a 0 ou 1 e retorna 1 nesses casos. Caso contrário, utiliza a recursão para calcular o fatorial.

No programa principal, utilizamos um laço `DO` para iterar de 1 até o número informado pelo usuário. A cada iteração, chamamos a função `calcular_fatorial` e armazenamos o resultado no vetor `fatorial`. Por fim, imprimimos na tela o vetor `fatorial` com os resultados dos fatoriais calculados.

Espero que esse código atenda às suas expectativas! Caso tenha mais alguma solicitação, não hesite em me perguntar.