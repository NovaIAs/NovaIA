Claro! Aqui está um código em FORTRAN que realiza o cálculo de Fibonacci usando a técnica de memoização para melhorar a eficiência.

```fortran
PROGRAM Fibonacci
  IMPLICIT NONE
  INTEGER :: n, i
  INTEGER, DIMENSION(:), ALLOCATABLE :: memo

  INTERFACE
    PURE FUNCTION fib(n) RESULT(result)
      INTEGER, INTENT(IN) :: n
      INTEGER :: result
    END FUNCTION fib
  END INTERFACE

  PRINT *, "Digite o valor de n: "
  READ *, n

  ALLOCATE(memo(0:n))
  memo = -1

  PRINT *, "O ", n, "º número de Fibonacci é: ", fib(n)

CONTAINS

  PURE FUNCTION fib(n) RESULT(result)
    INTEGER, INTENT(IN) :: n
    INTEGER :: result

    IF (n == 0) THEN
      result = 0
    ELSE IF (n == 1) THEN
      result = 1
    ELSE IF (memo(n) /= -1) THEN
      result = memo(n)
    ELSE
      memo(n) = fib(n-1) + fib(n-2)
      result = memo(n)
    END IF
  END FUNCTION fib

END PROGRAM Fibonacci
```

Neste código, começamos declarando a variável 'n' para guardar o valor de entrada fornecido pelo usuário. Em seguida, declaramos o array 'memo' que será usado para armazenar os valores já calculados de Fibonacci.

Em seguida, declaramos uma interface para a função 'fib', que será usada para calcular o n-ésimo número de Fibonacci. Essa interface é necessária porque a função 'fib' é recursiva e precisa ser definida após o programa principal.

Dentro do programa principal, pedimos ao usuário para digitar o valor de 'n' e alocamos memória para o array 'memo' com tamanho (0:n). Em seguida, inicializamos todos os elementos de 'memo' com o valor -1.

Após isso, chamamos a função 'fib(n)' para calcular o n-ésimo número de Fibonacci e imprimimos o resultado na tela.

A função 'fib' é definida usando a técnica de memoização. Ela verifica se o valor de 'n' é igual a 0 ou 1, caso seja, retorna diretamente o resultado. Caso contrário, verifica se o valor já foi calculado e armazenado em 'memo'. Se sim, retorna o valor armazenado. Caso contrário, calcula o valor recursivamente usando a relação de Fibonacci, armazena o resultado em 'memo' e retorna o valor.

Dessa forma, evitamos calcular repetidamente os mesmos valores de Fibonacci, melhorando a eficiência do código.