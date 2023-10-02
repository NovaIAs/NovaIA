Claro! Aqui está um código em FORTRAN que realiza uma simulação de um sistema de partículas em um espaço tridimensional.

```fortran
program simulacao_particulas
  implicit none
  integer, parameter :: num_particulas = 1000 ! número de partículas
  integer, parameter :: num_iteracoes = 1000 ! número de iterações
  real, parameter :: tempo = 1.0 ! tempo total da simulação
  real, parameter :: dt = tempo / num_iteracoes ! intervalo de tempo para cada iteração
  
  real :: posicao(num_particulas, 3) ! posição das partículas (x, y, z)
  real :: velocidade(num_particulas, 3) ! velocidade das partículas (vx, vy, vz)
  real :: forca(num_particulas, 3) ! força atuante sobre as partículas (fx, fy, fz)
  integer :: i, j, k
  
  ! Inicialização das posições e velocidades das partículas
  do i = 1, num_particulas
    do j = 1, 3
      posicao(i, j) = 10.0 * (2.0 * rand() - 1.0) ! posição inicial aleatória
      velocidade(i, j) = 0.0 ! velocidade inicial zero
    end do
  end do
  
  ! Laço principal da simulação
  do k = 1, num_iteracoes
    ! Cálculo das forças entre as partículas
    do i = 1, num_particulas
      do j = 1, num_particulas
        if (i /= j) then ! não calcular a força entre a mesma partícula
          forca(i, :) = forca(i, :) + calcular_forca(posicao(i, :), posicao(j, :))
        end if
      end do
    end do
    
    ! Atualização das posições e velocidades das partículas
    do i = 1, num_particulas
      velocidade(i, :) = velocidade(i, :) + (forca(i, :) * dt)
      posicao(i, :) = posicao(i, :) + (velocidade(i, :) * dt)
    end do
    
    ! Reset das forças para a próxima iteração
    forca = 0.0
  end do
  
  ! Exibição das posições finais das partículas
  do i = 1, num_particulas
    write(*, '(3F10.2)') posicao(i, :)
  end do
  
contains
  
  ! Função para calcular a força entre duas partículas
  function calcular_forca(posicao1, posicao2) result(forca)
    real, intent(in) :: posicao1(3), posicao2(3)
    real :: forca(3)
    
    ! Cálculo da força usando a lei de Coulomb
    real, parameter :: constante_eletrostatica = 9.0e9
    real :: distancia, distancia_cubica
    
    distancia = sqrt(sum((posicao2 - posicao1)**2))
    distancia_cubica = distancia**3
    
    forca = constante_eletrostatica * (posicao2 - posicao1) / distancia_cubica
  end function calcular_forca

end program simulacao_particulas
```

Esse código simula o movimento de partículas carregadas em um espaço tridimensional, utilizando a lei de Coulomb para calcular as forças entre as partículas. Ele realiza um número definido de iterações e exibe as posições finais das partículas.