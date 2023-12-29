Claro, vou lhe fornecer um exemplo de código complexo em FORTRAN que envolve cálculos matemáticos e manipulação de dados. Este código realiza a resolução numérica de um sistema de equações diferenciais ordinárias utilizando o método de Runge-Kutta de quarta ordem. Vou explicar o código passo a passo.

```fortran
PROGRAM RungeKutta
    IMPLICIT NONE
    
    INTEGER, PARAMETER :: n = 1000 ! Número de passos de tempo
    REAL, PARAMETER :: dt = 0.01   ! Tamanho do passo de tempo
    REAL, PARAMETER :: t0 = 0.0    ! Tempo inicial
    REAL, PARAMETER :: tF = 10.0   ! Tempo final
    
    REAL :: t(n), y(n), k1(n), k2(n), k3(n), k4(n)
    INTEGER :: i
    
    ! Função que define o sistema de equações diferenciais
    CONTAINS
    SUBROUTINE EquacoesDiferenciais(t, y, dydt)
        IMPLICIT NONE
        REAL, INTENT(IN) :: t
        REAL, INTENT(IN) :: y(n)
        REAL, INTENT(OUT) :: dydt(n)
        
        ! Equações diferenciais
        dydt(1) = -2.0 * t * y(1) + 2.0 * t**2
        dydt(2) = y(1)**2 + y(2)**2
        
    END SUBROUTINE EquacoesDiferenciais
    
    ! Subrotina que implementa o método de Runge-Kutta de quarta ordem
    CONTAINS
    SUBROUTINE RungeKutta4(t, y, dt)
        IMPLICIT NONE
        REAL, INTENT(INOUT) :: t
        REAL, INTENT(INOUT) :: y(n)
        REAL, INTENT(IN) :: dt
        
        INTEGER :: i
        
        DO i = 1, n
            CALL EquacoesDiferenciais(t, y, k1)
            CALL EquacoesDiferenciais(t + 0.5 * dt, y + 0.5 * dt * k1, k2)
            CALL EquacoesDiferenciais(t + 0.5 * dt, y + 0.5 * dt * k2, k3)
            CALL EquacoesDiferenciais(t + dt, y + dt * k3, k4)
            
            y = y + (dt / 6.0) * (k1 + 2.0 * k2 + 2.0 * k3 + k4)
            t = t + dt
        END DO
        
    END SUBROUTINE RungeKutta4
    
    ! Programa principal
    WRITE(*, '("Resolvendo um sistema de equacoes diferenciais utilizando o metodo de Runge-Kutta de 4a ordem")')
    
    ! Condições iniciais
    t(1) = t0
    y(1) = 0.0
    y(2) = 1.0
    
    CALL RungeKutta4(t, y, dt)
    
    ! Imprime os resultados
    DO i = 1, n
        WRITE(*, '(2F10.4)') t(i), y(i)
    END DO
    
END PROGRAM RungeKutta
```

Este código resolve um sistema de equações diferenciais ordinárias utilizando o método de Runge-Kutta de quarta ordem. O sistema de equações diferenciais é definido na subrotina `EquacoesDiferenciais`, onde as equações diferenciais são especificadas.

O programa principal começa definindo algumas constantes, como o número de passos de tempo (`n`), o tamanho do passo de tempo (`dt`), o tempo inicial (`t0`) e o tempo final (`tF`).

Em seguida, são declaradas as variáveis `t`, `y`, `k1`, `k2`, `k3` e `k4`, todas do tipo `REAL`, e a variável `i` do tipo `INTEGER`.

A subrotina `EquacoesDiferenciais` recebe como entrada o tempo (`t`), um vetor de valores das variáveis dependentes (`y`) e retorna um vetor com as derivadas das variáveis dependentes (`dydt`). Nesta subrotina, são especificadas as equações diferenciais que descrevem o sistema. No exemplo fornecido, temos um sistema de duas equações.

A subrotina `RungeKutta4` implementa o método de Runge-Kutta de quarta ordem. Ela recebe como entrada o tempo (`t`), o vetor de valores das variáveis dependentes (`y`) e o tamanho do passo de tempo (`dt`), e atualiza os valores das variáveis dependentes ao longo do tempo.

No programa principal, são definidas as condições iniciais para as variáveis dependentes (`y(1)` e `y(2)`), e em seguida é chamada a subrotina `RungeKutta4` para resolver o sistema de equações diferenciais.

Por fim, os resultados são impressos na tela, mostrando os valores das variáveis dependentes (`y`) em função do tempo (`t`), ao longo de todos os passos de tempo (`n`).