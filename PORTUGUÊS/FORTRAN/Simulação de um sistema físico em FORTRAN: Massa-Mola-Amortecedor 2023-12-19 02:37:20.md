Certamente! Aqui está um código em FORTRAN que realiza uma simulação de um sistema físico:

```fortran
PROGRAM SimulacaoFisica
  IMPLICIT NONE
  
  INTEGER :: i, j, n_iteracoes
  REAL :: tempo_total, delta_tempo, massa, constante_elastica, constante_amortecimento, posicao, velocidade, aceleracao
  
  ! Configurações iniciais
  tempo_total = 10.0
  delta_tempo = 0.01
  massa = 1.0
  constante_elastica = 2.0
  constante_amortecimento = 0.5
  
  ! Número de iterações
  n_iteracoes = INT(tempo_total / delta_tempo)
  
  ! Inicialização das variáveis
  posicao = 0.0
  velocidade = 0.0
  aceleracao = 0.0
  
  ! Loop principal da simulação
  DO i = 1, n_iteracoes
    ! Cálculo da aceleração
    aceleracao = -constante_elastica * posicao / massa - constante_amortecimento * velocidade
    
    ! Atualização das variáveis
    posicao = posicao + velocidade * delta_tempo + 0.5 * aceleracao * delta_tempo**2
    velocidade = velocidade + aceleracao * delta_tempo
    
    ! Imprimir os resultados a cada iteração
    WRITE(*,*) 'Iteração', i
    WRITE(*,*) 'Tempo:', i * delta_tempo
    WRITE(*,*) 'Posição:', posicao
    WRITE(*,*) 'Velocidade:', velocidade
    WRITE(*,*) 'Aceleração:', aceleracao
    WRITE(*,*)
  END DO
  
END PROGRAM SimulacaoFisica
```

Neste código, é realizada a simulação de um sistema físico massa-mola-amortecedor. A variável `tempo_total` é o tempo total de simulação desejado, `delta_tempo` é o intervalo de tempo entre cada iteração, `massa` é a massa do objeto, `constante_elastica` é a constante elástica da mola e `constante_amortecimento` é a constante de amortecimento do sistema.

O número de iterações é calculado com base no tempo total e no intervalo de tempo. A cada iteração, a posição, velocidade e aceleração são atualizadas de acordo com as equações do sistema físico. Os resultados são exibidos a cada iteração.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida adicional ou precisar de outros tipos de código, estarei aqui para ajudar.