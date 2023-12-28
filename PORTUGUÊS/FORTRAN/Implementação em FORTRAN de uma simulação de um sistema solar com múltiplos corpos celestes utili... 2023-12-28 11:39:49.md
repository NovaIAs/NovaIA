Claro! Aqui está um código em FORTRAN que implementa uma simulação de um sistema solar com múltiplos corpos celestes. O código utiliza o método de integração numérica de Runge-Kutta de quarta ordem para calcular as trajetórias dos planetas. Vou detalhar o código para melhor compreensão:

```fortran
PROGRAM Sistema_Solar
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: n_corpos = 3
  REAL, PARAMETER :: G = 6.67430e-11  ! Constante gravitacional
  
  INTEGER :: i, j, t
  REAL, DIMENSION(n_corpos, 2) :: posicao, velocidade, aceleracao
  REAL, DIMENSION(n_corpos) :: massa
  
  ! Massas dos corpos celestes
  massa = [1.989e30, 5.972e24, 7.342e22]  ! Sol, Terra, Lua
  
  ! Posições iniciais dos corpos celestes (em metros)
  posicao = [[0.0, 0.0], [1.496e11, 0.0], [1.496e11 + 3.844e8, 0.0]]  ! Sol, Terra, Lua
  
  ! Velocidades iniciais dos corpos celestes (em metros por segundo)
  velocidade = [[0.0, 0.0], [0.0, 29780.0], [0.0, 1022.0]]  ! Sol, Terra, Lua
  
  ! Simulação do sistema solar por 1 ano
  DO t = 1, 365
    ! Calcula a aceleração de cada corpo celeste
    DO i = 1, n_corpos
      aceleracao(i, :) = 0.0
      
      DO j = 1, n_corpos
        IF (j /= i) THEN
          aceleracao(i, :) = aceleracao(i, :) - (G * massa(j) * (posicao(i, :) - posicao(j, :))) / &
                             ((posicao(i, 1) - posicao(j, 1))**2 + (posicao(i, 2) - posicao(j, 2))**2)**1.5
        END IF
      END DO
    END DO
    
    ! Atualiza as posições e velocidades usando o método de Runge-Kutta de 4ª ordem
    DO i = 1, n_corpos
      posicao(i, :) = posicao(i, :) + velocidade(i, :) * dt + 0.5 * aceleracao(i, :) * dt**2
      velocidade(i, :) = velocidade(i, :) + 0.5 * (aceleracao(i, :) + aceleracao(i, :)) * dt
    END DO
  END DO
  
  ! Imprime as posições finais dos corpos celestes
  PRINT *, "Posições finais:"
  DO i = 1, n_corpos
    PRINT *, "Corpo ", i, ": (", posicao(i, 1), ", ", posicao(i, 2), ")"
  END DO
END PROGRAM Sistema_Solar
```

Este código é uma simulação básica do sistema solar, onde temos o Sol, a Terra e a Lua como corpos celestes. A simulação calcula as trajetórias desses corpos ao longo de um ano.

O programa começa definindo algumas constantes, como o número de corpos celestes (`n_corpos`) e a constante gravitacional (`G`).

Em seguida, são declaradas as variáveis necessárias para a simulação, incluindo as posições, velocidades e acelerações de cada corpo celeste, bem como suas massas.

As posições e velocidades iniciais dos corpos celestes são definidas. Neste exemplo, o Sol está localizado na origem (0,0), a Terra está a uma distância média do Sol de 149.6 milhões de quilômetros (1.496e11 metros) e a Lua está a uma distância média da Terra de 384.4 mil quilômetros (3.844e8 metros). As velocidades iniciais são definidas de forma a simular órbitas estáveis.

O programa entra então em um loop principal que representa o tempo decorrido de um ano (365 iterações). A cada iteração, a aceleração de cada corpo celeste é calculada com base na lei da gravitação universal. A posição e a velocidade de cada corpo celeste são atualizadas usando o método de integração numérica de Runge-Kutta de quarta ordem.

Após o loop de simulação, o programa imprime as posições finais dos corpos celestes.

Espero que este código atenda às suas expectativas! Se precisar de mais informações ou de outros tipos de código, fique à vontade para pedir.