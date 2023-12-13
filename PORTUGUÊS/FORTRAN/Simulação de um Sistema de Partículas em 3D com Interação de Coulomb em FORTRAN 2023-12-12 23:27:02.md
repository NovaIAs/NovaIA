Certamente! Aqui está um código em FORTRAN que realiza uma simulação de um sistema de partículas em 3D, onde cada partícula interage com as outras através da força de Coulomb. 

```fortran
program simulacao_particulas
  implicit none
  integer, parameter :: num_particulas = 100
  integer, parameter :: num_iteracoes = 1000
  real, parameter :: carga_elementar = 1.602e-19
  real, parameter :: constante_coulomb = 8.988e9
  real, parameter :: massa_particula = 9.109e-31
  real, parameter :: delta_t = 1.0e-9

  integer :: i, j, t
  real :: posicao_x(num_particulas), posicao_y(num_particulas), posicao_z(num_particulas)
  real :: velocidade_x(num_particulas), velocidade_y(num_particulas), velocidade_z(num_particulas)
  real :: aceleracao_x(num_particulas), aceleracao_y(num_particulas), aceleracao_z(num_particulas)
  
  ! Inicialização das posições e velocidades das partículas
  do i = 1, num_particulas
    posicao_x(i) = 0.0
    posicao_y(i) = 0.0
    posicao_z(i) = 0.0
    velocidade_x(i) = 0.0
    velocidade_y(i) = 0.0
    velocidade_z(i) = 0.0
  end do
  
  ! Loop principal para simular as interações entre as partículas
  do t = 1, num_iteracoes
    ! Zerar as acelerações a cada iteração
    aceleracao_x = 0.0
    aceleracao_y = 0.0
    aceleracao_z = 0.0
    
    ! Calcular as forças de Coulomb entre as partículas
    do i = 1, num_particulas
      do j = 1, num_particulas
        if (i /= j) then
          ! Calcular a distância entre as partículas
          real :: distancia_x, distancia_y, distancia_z, distancia_total, forca
        
          distancia_x = posicao_x(j) - posicao_x(i)
          distancia_y = posicao_y(j) - posicao_y(i)
          distancia_z = posicao_z(j) - posicao_z(i)
          distancia_total = sqrt(distancia_x**2 + distancia_y**2 + distancia_z**2)
          
          ! Calcular a força de Coulomb entre as partículas
          forca = constante_coulomb * carga_elementar**2 / distancia_total**2
          
          ! Calcular a aceleração causada pela força de Coulomb
          aceleracao_x(i) = aceleracao_x(i) + forca * distancia_x / (massa_particula * distancia_total)
          aceleracao_y(i) = aceleracao_y(i) + forca * distancia_y / (massa_particula * distancia_total)
          aceleracao_z(i) = aceleracao_z(i) + forca * distancia_z / (massa_particula * distancia_total)
        end if
      end do
    end do
    
    ! Atualizar as posições e velocidades das partículas usando o método de Verlet
    do i = 1, num_particulas
      posicao_x(i) = posicao_x(i) + velocidade_x(i) * delta_t + 0.5 * aceleracao_x(i) * delta_t**2
      posicao_y(i) = posicao_y(i) + velocidade_y(i) * delta_t + 0.5 * aceleracao_y(i) * delta_t**2
      posicao_z(i) = posicao_z(i) + velocidade_z(i) * delta_t + 0.5 * aceleracao_z(i) * delta_t**2
      
      velocidade_x(i) = velocidade_x(i) + 0.5 * (aceleracao_x(i) + aceleracao_x(i)) * delta_t
      velocidade_y(i) = velocidade_y(i) + 0.5 * (aceleracao_y(i) + aceleracao_y(i)) * delta_t
      velocidade_z(i) = velocidade_z(i) + 0.5 * (aceleracao_z(i) + aceleracao_z(i)) * delta_t
    end do
  end do
  
  ! Imprimir as posições finais das partículas
  do i = 1, num_particulas
    print *, "Particula ", i, ": (", posicao_x(i), ", ", posicao_y(i), ", ", posicao_z(i), ")"
  end do
  
end program simulacao_particulas
```

Neste código, iniciamos definindo algumas constantes como o número de partículas, o número de iterações da simulação, a carga elementar, a constante de Coulomb, a massa da partícula e o intervalo de tempo. Em seguida, declaramos as variáveis necessárias para as posições, velocidades e acelerações das partículas.

No loop principal, inicializamos as posições e velocidades das partículas e, em seguida, calculamos as forças de Coulomb entre todas as partículas. Para cada par de partículas, calculamos a distância entre elas e, em seguida, a força de Coulomb. A partir da força, calculamos a aceleração causada por essa força.

Após calcular todas as forças, atualizamos as posições e velocidades das partículas usando o método de Verlet. Esse método é uma técnica numérica para a integração de equações diferenciais que permite avançar no tempo usando posições, velocidades e acelerações.

Por fim, imprimimos as posições finais das partículas. Cada partícula é identificada por um número e suas coordenadas são apresentadas em formato (x, y, z).