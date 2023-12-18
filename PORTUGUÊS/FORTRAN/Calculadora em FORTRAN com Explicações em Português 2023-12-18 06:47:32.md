Claro! Aqui está um exemplo de um código complexo em FORTRAN, com explicações em português:

```
PROGRAM Calculadora
  IMPLICIT NONE

  INTEGER :: operacao, n1, n2, resultado
  CHARACTER(10) :: continuar

  ! Loop principal para executar várias operações
  DO
    ! Solicitar a operação ao usuário
    WRITE(*,*) "--------------------------------"
    WRITE(*,*) "Selecione a operação desejada:"
    WRITE(*,*) "1. Soma"
    WRITE(*,*) "2. Subtração"
    WRITE(*,*) "3. Multiplicação"
    WRITE(*,*) "4. Divisão"
    WRITE(*,*)
    WRITE(*,*) "Escolha a operação (1-4):"
    READ(*,*) operacao

    ! Solicitar os números ao usuário
    WRITE(*,*)
    WRITE(*,*) "Digite o primeiro número:"
    READ(*,*) n1
    WRITE(*,*) "Digite o segundo número:"
    READ(*,*) n2

    ! Executar a operação selecionada
    SELECT CASE (operacao)
      CASE (1)
        resultado = n1 + n2
        WRITE(*,*)
        WRITE(*,*) "Resultado da soma:", resultado
      CASE (2)
        resultado = n1 - n2
        WRITE(*,*)
        WRITE(*,*) "Resultado da subtração:", resultado
      CASE (3)
        resultado = n1 * n2
        WRITE(*,*)
        WRITE(*,*) "Resultado da multiplicação:", resultado
      CASE (4)
        IF (n2 /= 0) THEN
          resultado = n1 / n2
          WRITE(*,*)
          WRITE(*,*) "Resultado da divisão:", resultado
        ELSE
          WRITE(*,*)
          WRITE(*,*) "Erro: Divisão por zero!"
        END IF
      CASE DEFAULT
        WRITE(*,*)
        WRITE(*,*) "Opção inválida!"
    END SELECT

    ! Perguntar ao usuário se ele deseja continuar
    WRITE(*,*)
    WRITE(*,*) "Deseja realizar outra operação? (S/N):"
    READ(*,*) continuar
    IF (continuar /= 'S' .AND. continuar /= 's') THEN
      EXIT
    END IF
  END DO

END PROGRAM Calculadora
```

Explicação:
- O programa começa com a declaração das variáveis `operacao`, `n1`, `n2` e `resultado`. Essas variáveis serão usadas para armazenar os valores digitados pelo usuário e os resultados das operações.
- Em seguida, há um loop principal `DO` que permite ao usuário realizar várias operações consecutivas. O loop só será interrompido se o usuário escolher não continuar.
- Dentro do loop, o programa solicita ao usuário que selecione uma operação digitando um número de 1 a 4. O número da operação é armazenado na variável `operacao`.
- O programa também solicita ao usuário que digite os dois números nos quais a operação será realizada. Os números são armazenados nas variáveis `n1` e `n2`.
- Em seguida, o programa executa a operação selecionada pelo usuário usando uma estrutura `SELECT CASE`. Dependendo do valor de `operacao`, o programa realiza uma soma, subtração, multiplicação ou divisão e armazena o resultado na variável `resultado`.
- O programa exibe o resultado da operação na tela.
- Após exibir o resultado, o programa pergunta ao usuário se ele deseja realizar outra operação. Se o usuário digitar "S" ou "s", o loop continuará. Caso contrário, o loop será interrompido usando a instrução `EXIT`.
- O programa continua a repetir o loop até que o usuário decida não continuar.
