Claro! Aqui está um código em FORTRAN que realiza uma simulação de um problema de física. O programa calcula o movimento de um objeto em queda livre considerando a resistência do ar.

```
PROGRAM SimulacaoQuedaLivre
  IMPLICIT NONE

  INTEGER :: i, N
  REAL :: tempo, intervalo, massa, altura_inicial, velocidade_inicial, aceleracao_gravitacional
  REAL :: altura, velocidade, aceleracao, resistencia_ar, forca_resistencia

  ! Definindo os parâmetros iniciais
  N = 1000     ! Número de intervalos de tempo
  intervalo = 0.01   ! Intervalo de tempo em segundos
  massa = 1.0    ! Massa do objeto em quilogramas
  altura_inicial = 100.0    ! Altura inicial em metros
  velocidade_inicial = 0.0   ! Velocidade inicial em metros por segundo
  aceleracao_gravitacional = 9.8   ! Aceleração gravitacional em metros por segundo ao quadrado

  ! Calculando o movimento do objeto em queda livre
  altura = altura_inicial
  velocidade = velocidade_inicial

  DO i = 1, N
    tempo = i * intervalo
    aceleracao = aceleracao_gravitacional
    resistencia_ar = 0.5 * velocidade * velocidade * 1.2 * 0.5   ! Fórmula simplificada para a resistência do ar
    forca_resistencia = resistencia_ar / massa
    aceleracao = aceleracao - forca_resistencia

    altura = altura - velocidade * intervalo + 0.5 * aceleracao * intervalo * intervalo
    velocidade = velocidade + aceleracao * intervalo

    WRITE(*, '(A, F10.2, A, F10.2, A, F10.2)') "Tempo: ", tempo, " Altura: ", altura, " Velocidade: ", velocidade
  END DO

END PROGRAM SimulacaoQuedaLivre
```

Nesse código, a simulação de queda livre é realizada em um loop que itera sobre um número definido de intervalos de tempo. A altura, velocidade e aceleração do objeto são atualizadas a cada iteração, levando em consideração a aceleração gravitacional e a resistência do ar.

A altura inicial, velocidade inicial, massa, aceleração gravitacional, número de intervalos de tempo e intervalo de tempo são parâmetros que podem ser ajustados de acordo com a necessidade da simulação.

O código imprime na tela o tempo, a altura e a velocidade do objeto em cada intervalo de tempo.