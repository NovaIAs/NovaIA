Claro! Aqui está um exemplo de código em FORTRAN que realiza a simulação de um sistema de partículas em um campo magnético:

```fortran
program simulacao_particulas
  implicit none
  
  integer, parameter :: num_particulas = 1000 ! número de partículas a serem simuladas
  integer, parameter :: passos_simulacao = 1000 ! número de passos da simulação
  real, parameter :: dt = 0.01 ! intervalo de tempo entre cada passo da simulação
  
  integer :: i, j, t ! variáveis de controle
  
  real, dimension(num_particulas) :: posicao_x, posicao_y, velocidade_x, velocidade_y ! vetores das posições e velocidades das partículas
  real, dimension(num_particulas) :: forca_x, forca_y ! vetores das forças atuantes sobre as partículas
  
  ! Inicialização das posições e velocidades das partículas
  do i = 1, num_particulas
    posicao_x(i) = 0.0
    posicao_y(i) = 0.0
    velocidade_x(i) = 1.0
    velocidade_y(i) = 1.0
  end do
  
  ! Loop principal da simulação
  do t = 1, passos_simulacao
    ! Cálculo das forças magnéticas
    do i = 1, num_particulas
      forca_x(i) = 0.0
      forca_y(i) = 0.0
      
      do j = 1, num_particulas
        if (j /= i) then
          ! Cálculo da distância entre as partículas
          real :: distancia_x, distancia_y, distancia
          
          distancia_x = posicao_x(j) - posicao_x(i)
          distancia_y = posicao_y(j) - posicao_y(i)
          distancia = sqrt(distancia_x**2 + distancia_y**2)
          
          ! Cálculo da força magnética
          real :: forca_magnetica
          
          forca_magnetica = 1.0 / distancia**2
          
          ! Atualização das forças atuantes
          forca_x(i) = forca_x(i) + forca_magnetica * distancia_x
          forca_y(i) = forca_y(i) + forca_magnetica * distancia_y
        end if
      end do
    end do
    
    ! Atualização das posições e velocidades das partículas
    do i = 1, num_particulas
      posicao_x(i) = posicao_x(i) + velocidade_x(i) * dt
      posicao_y(i) = posicao_y(i) + velocidade_y(i) * dt
      
      velocidade_x(i) = velocidade_x(i) + forca_x(i) * dt
      velocidade_y(i) = velocidade_y(i) + forca_y(i) * dt
    end do
  end do
  
  ! Impressão das posições finais das partículas
  do i = 1, num_particulas
    write(*,*) "Partícula", i, ":", posicao_x(i), posicao_y(i)
  end do
  
end program simulacao_particulas
```

Este código em FORTRAN simula um sistema de partículas em um campo magnético. Ele utiliza um loop principal para iterar sobre um determinado número de passos de simulação. Em cada passo, ele calcula as forças magnéticas entre as partículas e atualiza suas posições e velocidades de acordo com essas forças.

O código começa definindo algumas constantes, como o número de partículas a serem simuladas, o número de passos da simulação e o intervalo de tempo entre cada passo. Em seguida, são declaradas as variáveis e vetores necessários para armazenar as posições, velocidades e forças das partículas.

Dentro do loop principal, há um loop aninhado que calcula as forças magnéticas entre todas as combinações de partículas. Os cálculos são feitos com base na distância entre as partículas usando a fórmula da força magnética. As forças são acumuladas nos vetores `forca_x` e `forca_y`.

Após o cálculo das forças, outro loop atualiza as posições e velocidades das partículas com base nas forças e no intervalo de tempo `dt`. Finalmente, o código imprime as posições finais das partículas.

Espero que este código atenda às suas expectativas! Se você tiver alguma dúvida ou precisar de mais informações, fique à vontade para perguntar.