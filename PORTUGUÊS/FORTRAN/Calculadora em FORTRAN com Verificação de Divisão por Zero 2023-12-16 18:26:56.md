Certamente! Aqui está um código complexo em FORTRAN, explicando cada parte:

```fortran
PROGRAM Calculadora
  IMPLICIT NONE

  INTEGER :: a, b, resultado
  CHARACTER(1) :: operacao

  ! Solicita ao usuário que insira dois números inteiros e a operação desejada
  WRITE(*,*) "Digite o primeiro número inteiro:"
  READ(*,*) a

  WRITE(*,*) "Digite o segundo número inteiro:"
  READ(*,*) b

  WRITE(*,*) "Digite a operação desejada (+, -, *, /):"
  READ(*,*) operacao

  ! Realiza a operação desejada com base nos números inseridos
  SELECT CASE (operacao)
    CASE ('+')
      resultado = a + b
    CASE ('-')
      resultado = a - b
    CASE ('*')
      resultado = a * b
    CASE ('/')
      IF (b /= 0) THEN
        resultado = a / b
      ELSE
        WRITE(*,*) "Erro: Divisão por zero não é permitida."
      END IF
    CASE DEFAULT
      WRITE(*,*) "Erro: Operação inválida."
  END SELECT

  ! Imprime o resultado da operação ou mensagem de erro, se houver
  IF (operacao == '/') THEN
    IF (b /= 0) THEN
      WRITE(*,*) "Resultado: ", resultado
    END IF
  ELSE
    WRITE(*,*) "Resultado: ", resultado
  END IF

END PROGRAM Calculadora
```

Neste programa em FORTRAN, você poderá inserir dois números inteiros e a operação desejada (+, -, *, /). O programa então realizará a operação escolhida e exibirá o resultado na tela. Se a operação for a divisão (/), o programa verificará se o divisor é diferente de zero antes de realizar a divisão para evitar erros. Caso o divisor seja zero, uma mensagem de erro será exibida.

Espero que isso atenda às suas necessidades! Se você tiver mais alguma solicitação, por favor, me avise.