Claro! Aqui está um exemplo de um código complexo em FORTRAN, que implementa uma simulação de um modelo de propagação de doenças em uma população:

```fortran
PROGRAM Simulacao_Doenca
  IMPLICIT NONE

  INTEGER, PARAMETER :: N = 1000 ! Tamanho da população
  INTEGER, PARAMETER :: T = 100 ! Número de dias de simulação
  INTEGER, PARAMETER :: NUM_INFECTADOS_INICIAL = 10 ! Número inicial de infectados
  REAL, PARAMETER :: TAXA_CONTAGIO = 0.2 ! Taxa de contágio
  REAL, PARAMETER :: TAXA_RECUPERACAO = 0.1 ! Taxa de recuperação

  INTEGER :: i, j ! Variáveis de controle
  INTEGER :: num_infectados, num_recuperados, num_saudaveis ! Número de indivíduos em cada estado
  INTEGER :: populacao(N) ! Vetor que representa a população
  INTEGER :: infectados_dia(T) ! Vetor que armazena o número de infectados em cada dia

  ! Inicialização da população
  populacao = 0
  DO i = 1, NUM_INFECTADOS_INICIAL
    j = INT(RAND()*N) + 1
    populacao(j) = 1 ! 1 representa o estado 'infectado'
  END DO

  ! Simulação da propagação da doença
  DO i = 1, T
    num_infectados = COUNT(populacao == 1)
    num_recuperados = COUNT(populacao == 2)
    num_saudaveis = COUNT(populacao == 0)
    infectados_dia(i) = num_infectados

    WRITE(*,*) 'Dia', i
    WRITE(*,*) 'Número de infectados:', num_infectados
    WRITE(*,*) 'Número de recuperados:', num_recuperados
    WRITE(*,*) 'Número de indivíduos saudáveis:', num_saudaveis
    WRITE(*,*)

    ! Atualização do estado da população
    DO j = 1, N
      IF (populacao(j) == 1) THEN
        populacao(j) = 2 ! 2 representa o estado 'recuperado'
      ELSE IF (populacao(j) == 0) THEN
        DO WHILE (populacao(j) /= 0 .AND. populacao(j) /= 1)
          IF (RAND() < TAXA_CONTAGIO * num_infectados / N) THEN
            populacao(j) = 1 ! Indivíduo foi infectado
          END IF
        END DO
      END IF
    END DO
  END DO

  ! Impressão do número de infectados em cada dia
  WRITE(*,*)
  WRITE(*,*) 'Número de infectados em cada dia:'
  DO i = 1, T
    WRITE(*,'(I3, A, I5)') i, ':', infectados_dia(i)
  END DO

END PROGRAM Simulacao_Doenca
```

Neste código, simulamos o modelo de propagação de uma doença em uma população de tamanho N durante T dias. A população é representada pelo vetor `populacao`, onde o valor 0 indica que um indivíduo está saudável, o valor 1 indica que está infectado e o valor 2 indica que está recuperado.

O código começa inicializando a população com um número inicial de infectados (`NUM_INFECTADOS_INICIAL`). Em cada dia de simulação, atualizamos o estado de cada indivíduo com base na taxa de contágio (`TAXA_CONTAGIO`) e na taxa de recuperação (`TAXA_RECUPERACAO`). O número de infectados em cada dia é armazenado no vetor `infectados_dia`.

Ao final da simulação, o código imprime o número de infectados em cada dia.