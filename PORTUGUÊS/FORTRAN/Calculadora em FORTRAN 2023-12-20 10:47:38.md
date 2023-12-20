PROGRAM Calculadora
  IMPLICIT NONE

  INTEGER :: opcao, num1, num2, resultado
  REAL :: resultado_divisao
  CHARACTER(10) :: operacao

  PRINT*, "Bem-vindo(a) à calculadora em FORTRAN!"

  DO WHILE (opcao /= 5)
    PRINT*, ""
    PRINT*, "Selecione uma operação:"
    PRINT*, "1. Soma"
    PRINT*, "2. Subtração"
    PRINT*, "3. Multiplicação"
    PRINT*, "4. Divisão"
    PRINT*, "5. Sair"
    PRINT*, ""
    PRINT*, "Opção: "
    READ*, opcao

    SELECT CASE (opcao)
      CASE (1)
        operacao = "soma"
        PRINT*, "Digite o primeiro número: "
        READ*, num1
        PRINT*, "Digite o segundo número: "
        READ*, num2
        resultado = num1 + num2
        PRINT*, "O resultado da " // operacao // " é ", resultado
      CASE (2)
        operacao = "subtração"
        PRINT*, "Digite o primeiro número: "
        READ*, num1
        PRINT*, "Digite o segundo número: "
        READ*, num2
        resultado = num1 - num2
        PRINT*, "O resultado da " // operacao // " é ", resultado
      CASE (3)
        operacao = "multiplicação"
        PRINT*, "Digite o primeiro número: "
        READ*, num1
        PRINT*, "Digite o segundo número: "
        READ*, num2
        resultado = num1 * num2
        PRINT*, "O resultado da " // operacao // " é ", resultado
      CASE (4)
        operacao = "divisão"
        PRINT*, "Digite o primeiro número: "
        READ*, num1
        PRINT*, "Digite o segundo número: "
        READ*, num2
        resultado_divisao = REAL(num1) / num2
        PRINT*, "O resultado da " // operacao // " é ", resultado_divisao
      CASE (5)
        PRINT*, "Saindo da calculadora..."
      CASE DEFAULT
        PRINT*, "Opção inválida! Tente novamente."
    END SELECT
  END DO

  PRINT*, "Obrigado por usar a calculadora em FORTRAN!"
  
END PROGRAM Calculadora
