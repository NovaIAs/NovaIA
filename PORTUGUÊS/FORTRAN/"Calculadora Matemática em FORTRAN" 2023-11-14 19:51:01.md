Aqui está o código em FORTRAN:

```
PROGRAM CalculadoraMatematica
    IMPLICIT NONE
    
    INTEGER :: n, opcao
    INTEGER, DIMENSION(100) :: numeros
    REAL :: resultado
    
    WRITE(*, "('Bem-vindo à Calculadora Matemática')")

    ! Receber a quantidade de números a serem inseridos
    WRITE(*, "('Digite a quantidade de números a serem inseridos:')")
    READ(*, *) n

    ! Receber os números do usuário
    WRITE(*, "('Digite os', n, 'números:')")
    DO i = 1, n
        READ(*, *) numeros(i)
    END DO

    ! Exibir as opções de operação
    WRITE(*, "('Escolha uma opção de operação:')")
    WRITE(*, "('1. Soma')")
    WRITE(*, "('2. Subtração')")
    WRITE(*, "('3. Multiplicação')")
    WRITE(*, "('4. Divisão')")
    READ(*, *) opcao

    ! Realizar a operação selecionada
    SELECT CASE (opcao)
        CASE (1)
            resultado = 0.0
            DO i = 1, n
                resultado = resultado + numeros(i)
            END DO
            WRITE(*, "('O resultado da soma é:', F6.2)") resultado

        CASE (2)
            resultado = numeros(1)
            DO i = 2, n
                resultado = resultado - numeros(i)
            END DO
            WRITE(*, "('O resultado da subtração é:', F6.2)") resultado

        CASE (3)
            resultado = 1.0
            DO i = 1, n
                resultado = resultado * numeros(i)
            END DO
            WRITE(*, "('O resultado da multiplicação é:', F6.2)") resultado

        CASE (4)
            resultado = REAL(numeros(1))
            DO i = 2, n
                resultado = resultado / REAL(numeros(i))
            END DO
            WRITE(*, "('O resultado da divisão é:', F6.2)") resultado

        CASE DEFAULT
            WRITE(*, "('Opção inválida.')")

    END SELECT

    STOP
END PROGRAM CalculadoraMatematica
```

Este código em FORTRAN é uma calculadora matemática básica que permite ao usuário inserir uma quantidade de números desejada e realizar diferentes operações matemáticas (soma, subtração, multiplicação e divisão) com os números inseridos.

A primeira parte do código solicita ao usuário a quantidade de números a serem inseridos e, em seguida, o usuário digita os números. Depois disso, o código exibe as opções de operação disponíveis.

Dependendo da opção selecionada pelo usuário, o código realiza a operação correspondente utilizando os números fornecidos. O resultado é exibido no console.

Se o usuário escolher uma opção inválida, o código exibirá uma mensagem de erro.

É importante ressaltar que FORTRAN é uma linguagem antiga e não é amplamente utilizada atualmente. No entanto, a estrutura do código oferece uma base compreensível para entender como realizar operações matemáticas básicas em FORTRAN.

Espero que isso seja útil! Se você tiver alguma outra solicitação, fique à vontade para fazer.