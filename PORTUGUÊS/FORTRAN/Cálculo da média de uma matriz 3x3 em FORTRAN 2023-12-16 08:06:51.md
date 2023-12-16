Claro! Aqui está um exemplo de código em FORTRAN:

```fortran
! Este programa calcula a média de uma matriz 3x3

program media_matriz
  implicit none

  integer :: i, j
  real :: matriz(3, 3)
  real :: soma, media

  ! Lendo os valores da matriz
  do i = 1, 3
    do j = 1, 3
      write(*, '(a, i0, a, i0)') "Digite o valor da posição (", i, ",", j, "): "
      read(*, *) matriz(i, j)
    end do
  end do

  ! Calculando a soma dos elementos da matriz
  soma = 0.0
  do i = 1, 3
    do j = 1, 3
      soma = soma + matriz(i, j)
    end do
  end do

  ! Calculando a média dos elementos da matriz
  media = soma / 9.0

  ! Imprimindo o resultado
  write(*, *) "A média dos elementos da matriz é: ", media

end program media_matriz
```

Neste código, começamos declarando as variáveis necessárias, incluindo uma matriz 3x3 chamada "matriz", uma variável para armazenar a soma dos elementos e outra para a média.

Em seguida, usamos dois loops "do" aninhados para ler os valores da matriz do usuário.

Após a leitura, usamos novamente dois loops "do" aninhados para calcular a soma dos elementos da matriz.

Finalmente, dividimos a soma pelo número total de elementos (9, no caso de uma matriz 3x3) para obter a média. Imprimimos a média na tela.

Espero que este exemplo tenha sido útil! Se tiver alguma outra solicitação, é só pedir.